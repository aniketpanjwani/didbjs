.didbjs_sparse_dense_fallback_max_entries <- 5e6
.didbjs_scratch_column_stems <- c("v_star", "tau_bar", "recentered", "leaveout_scale")

did_imputation <- function(data,
                           y,
                           i,
                           t,
                           Ei,
                           controls = NULL,
                           fe = NULL,
                           aw = NULL,
                           cluster = NULL,
                           subset = NULL,
                           avgeffectsby = NULL,
                           leaveout = FALSE,
                           saveweights = FALSE,
                           loadweights = NULL,
                           saveestimates = FALSE,
                           saveresid = FALSE,
                           hetby = NULL,
                           project = NULL,
                           minn = 30,
                           autosample = FALSE,
                           wtr = NULL,
                           sum = FALSE,
                           horizons = NULL,
                           allhorizons = FALSE,
                           hbalance = FALSE,
                           pretrends = 0,
                           shift = 0,
                           delta = NULL,
                           significance_level = 0.05,
                           ...) {
  dots <- list(...)
  if (length(dots) > 0) {
    stop_unsupported("Unsupported arguments: ", paste(names(dots), collapse = ", "))
  }
  controls <- normalize_controls(controls)
  avgeffectsby <- normalize_avgeffectsby(avgeffectsby)
  leaveout <- normalize_flag(leaveout, "leaveout")
  saveweights <- normalize_flag(saveweights, "saveweights")
  saveestimates <- normalize_flag(saveestimates, "saveestimates")
  saveresid <- normalize_flag(saveresid, "saveresid")
  hetby <- normalize_single_column(hetby, "hetby")
  project <- normalize_controls(project)
  allhorizons <- normalize_allhorizons(allhorizons)
  hbalance <- normalize_flag(hbalance, "hbalance")
  sum_estimand <- normalize_flag(sum, "sum")
  pretrends <- normalize_nonnegative_integer_scalar(pretrends, "pretrends")
  shift <- normalize_integer_scalar(shift, "shift")
  delta <- normalize_delta(delta)
  significance_level <- normalize_significance_level(significance_level)
  minn <- normalize_nonnegative_integer_scalar(minn, "minn")
  wtr_cols <- normalize_wtr(wtr)
  if (isTRUE(sum_estimand) && length(wtr_cols) == 0) {
    stop_unsupported("Sum estimands without custom wtr are not implemented yet.")
  }
  if (isTRUE(sum_estimand) && isTRUE(autosample)) {
    stop_contract("Autosample cannot be combined with sum. Please specify the sample explicitly.")
  }
  if (isTRUE(allhorizons) && !is.null(horizons) && length(horizons) > 0) {
    stop_contract("Options horizons and allhorizons cannot be combined.")
  }
  if (length(wtr_cols) > 0 && (!is.null(horizons) || isTRUE(allhorizons) || isTRUE(hbalance))) {
    stop_contract("Custom wtr cannot be combined with horizons, allhorizons, or hbalance in the current conformance path.")
  }
  if (!is.null(hetby) && length(project) > 0) {
    stop_contract("Options project and hetby cannot be combined.")
  }
  if (length(project) > 0 && isTRUE(autosample)) {
    stop_contract("Autosample cannot be combined with project. Please specify the sample explicitly")
  }
  if (length(project) > 0 && isTRUE(sum_estimand)) {
    stop_contract("Options project and sum cannot be combined.")
  }
  if (length(project) > 0 && length(wtr_cols) > 0) {
    stop_contract("The option project can be combined with horizons/allhorizons but not with wtr.")
  }
  if (isTRUE(hbalance) && isTRUE(autosample)) {
    stop_contract("autosample cannot be combined with hbalance.")
  }
  if (isTRUE(hbalance) && !isTRUE(allhorizons) && (is.null(horizons) || length(horizons) == 0)) {
    stop_contract("hbalance requires horizons or allhorizons.")
  }
  horizons <- normalize_horizons(horizons)
  if (!minn %in% c(0L, 30L)) {
    stop_unsupported("Only minn = 30 and minn = 0 are currently accepted.")
  }

  cluster <- cluster %||% i
  if (missing(fe)) {
    fe_terms <- c(i, t)
  } else if (is.null(fe)) {
    fe_terms <- character()
  } else if (length(fe) == 0) {
    stop_contract("Empty fe vectors are invalid. Use fe = NULL for constant-only.")
  } else {
    fe_terms <- as.character(fe)
  }
  fe_required <- fixed_effect_source_columns(fe_terms)
  required <- c(y, i, t, Ei, controls, avgeffectsby, hetby, project, fe_required)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop_contract("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  if (!is.null(aw) && !aw %in% names(data)) {
    stop_contract("Missing analytic weight column: ", aw)
  }
  if (!is.null(cluster) && !cluster %in% names(data)) {
    stop_contract("Missing cluster column: ", cluster)
  }
  missing_wtr <- setdiff(wtr_cols, names(data))
  if (length(missing_wtr) > 0) {
    stop_contract("Missing custom wtr columns: ", paste(missing_wtr, collapse = ", "))
  }

  original_dt <- data.table::as.data.table(data.table::copy(data))
  original_row_id <- if ("row_id" %in% names(original_dt)) as.character(original_dt[["row_id"]]) else as.character(seq_len(nrow(original_dt)))
  validate_unique_row_id(original_row_id)
  subset_mask <- normalize_subset_mask(subset, original_dt)
  complete_cols <- unique(c(y, i, t, controls, cluster, aw, wtr_cols, avgeffectsby, hetby, project, fe_required))
  complete_mask <- if (length(complete_cols) == 0) {
    rep(TRUE, nrow(original_dt))
  } else {
    stats::complete.cases(original_dt[, ..complete_cols])
  }
  inclusion_mask <- subset_mask & complete_mask
  if (!any(inclusion_mask)) {
    stop_contract("No observations remain after applying subset and missing-value filters.")
  }
  validate_unique_unit_time(original_dt[inclusion_mask == TRUE], original_row_id[inclusion_mask], i, t)
  dt <- data.table::copy(original_dt[inclusion_mask == TRUE])
  row_id <- original_row_id[inclusion_mask]

  fe_contract <- prepare_fixed_effects(dt, fe_terms)
  fe_cols <- fe_contract$cols
  nonnumeric_controls <- controls[!vapply(controls, function(col) is.numeric(dt[[col]]), logical(1))]
  if (length(nonnumeric_controls) > 0) {
    stop_contract("Continuous controls must be numeric: ", paste(nonnumeric_controls, collapse = ", "))
  }
  nonnumeric_project <- project[!vapply(project, function(col) is.numeric(dt[[col]]), logical(1))]
  if (length(nonnumeric_project) > 0) {
    stop_contract("Project variables must be numeric: ", paste(nonnumeric_project, collapse = ", "))
  }

  timing <- dt[[Ei]]
  if (any(!is.na(timing) & is.finite(timing) & timing == 0)) {
    stop_contract("R-native API rejects zero treatment timing; use a Kyle compatibility wrapper for zero-as-never-treated semantics.")
  }
  validate_treatment_timing(dt, i, t, Ei)
  treatment_time <- dt[[Ei]]
  calendar_time <- dt[[t]]
  dt[, .didbjs_event_time := treatment_event_time(calendar_time, treatment_time, shift = shift, delta = delta)]
  if (any(!is.na(dt$.didbjs_event_time) & dt$.didbjs_event_time != as.integer(dt$.didbjs_event_time))) {
    stop_contract("There are non-integer values of the number of periods since treatment. Please check t, Ei, shift, and delta.")
  }
  dt[, .didbjs_untreated := is.na(.didbjs_event_time) | .didbjs_event_time < 0]
  dt[, .didbjs_treated := !.didbjs_untreated]
  if (!any(dt$.didbjs_untreated)) {
    stop_contract("No untreated observations are available for first-stage imputation.")
  }
  if (!any(dt$.didbjs_treated)) {
    stop_contract("No treated observations are available for static ATT.")
  }
  dt[, .didbjs_weight := if (is.null(aw)) 1 else dt[[aw]]]
  if (any(!is.finite(dt$.didbjs_weight) | dt$.didbjs_weight <= 0, na.rm = TRUE)) {
    stop_contract("Analytic weights must be positive and finite.")
  }
  if (!is.null(aw) && !isTRUE(sum_estimand)) {
    dt[, .didbjs_weight := .didbjs_weight * .N / sum(.didbjs_weight)]
  }

  formula <- first_stage_formula(y, controls, fe_cols)
  first_stage <- fixest::feols(
    formula,
    data = dt[.didbjs_untreated == TRUE],
    weights = ~.didbjs_weight,
    warn = FALSE,
    notes = FALSE,
    fixef.rm = "none",
    fixef.tol = 1e-10,
    fixef.iter = 100000
  )
  control_estimates <- first_stage_control_coefficients(first_stage, controls)

  dt[, .didbjs_y_hat := as.numeric(stats::predict(first_stage, newdata = dt))]
  if (isTRUE(allhorizons)) {
    horizons <- discover_all_horizons(dt)
  }
  weight_contract <- build_treatment_weights(
    dt,
    horizons,
    hbalance = hbalance,
    unit_col = i,
    custom_wtr = wtr_cols,
    sum_estimand = sum_estimand,
    hetby = hetby,
    project = project
  )
  dt <- weight_contract$data
  needs_imputation <- treatment_rows_with_nonzero_weights(dt, weight_contract$columns)
  dt[, .didbjs_needs_imputation := .didbjs_treated & needs_imputation]
  dt[, .didbjs_cannot_impute := is.na(.didbjs_y_hat) & .didbjs_needs_imputation]

  autosample_drop <- character()
  autosample_trim <- character()
  if (any(dt$.didbjs_cannot_impute)) {
    failed <- row_id[dt$.didbjs_cannot_impute]
    if (!isTRUE(autosample)) {
      stop_contract("Could not impute treated observations: ", paste(failed, collapse = ", "))
    }
    autosample_contract <- apply_autosample(
      dt = dt,
      terms = weight_contract$terms,
      columns = weight_contract$columns
    )
    dt <- autosample_contract$data
    autosample_drop <- autosample_contract$drop_terms
    autosample_trim <- autosample_contract$trim_terms
  }

  needs_imputation <- treatment_rows_with_nonzero_weights(dt, weight_contract$columns)
  dt[, .didbjs_needs_imputation := .didbjs_treated & needs_imputation]
  sample_mask <- (dt$.didbjs_untreated | dt$.didbjs_needs_imputation) &
    !(isTRUE(autosample) & dt$.didbjs_cannot_impute)

  suppression_contract <- apply_minn_suppression(
    dt = dt,
    terms = weight_contract$terms,
    columns = weight_contract$columns,
    sample_mask = sample_mask,
    minn = minn
  )
  dt <- suppression_contract$data
  post_suppression_needs <- treatment_rows_with_nonzero_weights(dt, weight_contract$columns)
  post_suppression_sample_mask <- (dt$.didbjs_untreated | (dt$.didbjs_treated & post_suppression_needs)) &
    !(isTRUE(autosample) & dt$.didbjs_cannot_impute)
  analysis_dt <- dt[sample_mask == TRUE]
  analysis_row_id <- row_id[sample_mask]
  data.table::set(
    analysis_dt,
    j = ".didbjs_tau",
    value = analysis_dt[[y]] - analysis_dt[[".didbjs_y_hat"]]
  )
  if (anyNA(analysis_dt$.didbjs_tau)) {
    stop_contract("Could not impute treated observations after applying the sample mask.")
  }
  n_treated <- vapply(
    weight_contract$columns,
    function(wtr_col) sum(analysis_dt$.didbjs_treated & analysis_dt[[wtr_col]] != 0),
    integer(1)
  )

  estimates_vector <- vapply(
    weight_contract$columns,
    function(wtr_col) analysis_dt[.didbjs_treated == TRUE, sum(get(wtr_col) * .didbjs_tau)],
    numeric(1)
  )
  names(estimates_vector) <- weight_contract$terms
  suppressed <- weight_contract$terms %in% suppression_contract$droplist
  pretrend_contract <- estimate_pretrends(
    dt = analysis_dt,
    y = y,
    controls = controls,
    fe_cols = fe_cols,
    cluster = cluster,
    pretrends = pretrends
  )
  weight_spec <- NULL
  if (isTRUE(saveweights) || !is.null(loadweights)) {
    weight_spec_cols <- unique(c(i, t, Ei, controls, fe_required, cluster, aw, wtr_cols, avgeffectsby, hetby, project))
    weight_spec <- saved_weight_spec(
      row_id = analysis_row_id,
      sample_signature = saved_weight_sample_signature(analysis_dt, analysis_row_id, weight_spec_cols),
      terms = weight_contract$terms,
      i = i,
      t = t,
      Ei = Ei,
      controls = controls,
      fe = fe_contract$terms,
      aw = aw,
      cluster = cluster,
      wtr = wtr_cols,
      sum = isTRUE(sum_estimand),
      hetby = hetby %||% "",
      project = project,
      horizons = horizons,
      allhorizons = isTRUE(allhorizons),
      hbalance = isTRUE(hbalance),
      avgeffectsby = avgeffectsby %||% c(Ei, t),
      leaveout = isTRUE(leaveout),
      autosample = isTRUE(autosample),
      minn = minn,
      shift = shift,
      delta = delta
    )
  }
  loadweights_contract <- prepare_loadweights(
    loadweights = loadweights,
    row_id = analysis_row_id,
    terms = weight_contract$terms,
    expected_spec_hash = weight_spec$spec_hash %||% ""
  )
  covariance_contract <- static_cluster_covariance(
    dt = analysis_dt,
    first_stage = first_stage,
    cluster = cluster,
    Ei = Ei,
    terms = weight_contract$terms,
    wtr_cols = weight_contract$columns,
    controls = controls,
    fe_cols = fe_cols,
    avgeffectsby = avgeffectsby %||% c(Ei, ".didbjs_event_time"),
    leaveout = isTRUE(leaveout),
    loadweights = loadweights_contract$matrix,
    extra_cluster_scores = pretrend_contract$cluster_scores,
    extra_terms = pretrend_contract$terms
  )
  covariance <- covariance_contract$covariance
  std_errors <- sqrt(diag(covariance)[weight_contract$terms])
  display_estimates <- estimates_vector
  display_std_errors <- std_errors
  display_estimates[suppressed] <- NA_real_
  display_std_errors[suppressed] <- NA_real_
  control_std_errors <- if (length(controls) > 0) sqrt(diag(covariance)[controls]) else numeric()
  z <- stats::qnorm(1 - significance_level / 2)
  estimates <- data.frame(
    term = weight_contract$terms,
    estimate = unname(display_estimates),
    std.error = unname(display_std_errors),
    conf.low = unname(display_estimates - z * display_std_errors),
    conf.high = unname(display_estimates + z * display_std_errors),
    n_obs = sum(sample_mask),
    n_control = sum(dt$.didbjs_untreated & sample_mask),
    n_treated = unname(n_treated),
    stringsAsFactors = FALSE
  )
  if (length(pretrend_contract$terms) > 0) {
    pre_std_errors <- sqrt(diag(covariance)[pretrend_contract$terms])
    pre_estimates <- data.frame(
      term = pretrend_contract$terms,
      estimate = unname(pretrend_contract$estimates),
      std.error = unname(pre_std_errors),
      conf.low = unname(pretrend_contract$estimates - z * pre_std_errors),
      conf.high = unname(pretrend_contract$estimates + z * pre_std_errors),
      n_obs = sum(sample_mask),
      n_control = sum(dt$.didbjs_untreated & sample_mask),
      n_treated = NA_integer_,
      stringsAsFactors = FALSE
    )
    estimates <- rbind(estimates, pre_estimates)
  }
  control_table <- data.frame(
    term = controls,
    estimate = unname(control_estimates),
    std.error = unname(control_std_errors),
    conf.low = unname(control_estimates - z * control_std_errors),
    conf.high = unname(control_estimates + z * control_std_errors),
    stringsAsFactors = FALSE
  )
  full_sample_mask <- rep(FALSE, length(original_row_id))
  full_sample_mask[inclusion_mask] <- sample_mask
  full_cannot_impute <- rep(FALSE, length(original_row_id))
  full_cannot_impute[inclusion_mask] <- dt$.didbjs_cannot_impute

  result <- list(
    estimates = estimates,
    controls = control_table,
    covariance = covariance,
    sample_mask = data.frame(
      row_id = original_row_id,
      sample = full_sample_mask,
      cannot_impute = full_cannot_impute,
      subset = subset_mask,
      missing_required = !complete_mask,
      stringsAsFactors = FALSE
    ),
    artifacts = list(),
    diagnostics = list(
      command = "did_imputation",
      fe = fe_contract$terms,
      controls = controls,
      cluster = cluster,
      subset = !is.null(subset),
      subset_excluded_row_ids = original_row_id[!subset_mask],
      missing_excluded_row_ids = original_row_id[subset_mask & !complete_mask],
      avgeffectsby = avgeffectsby %||% c(Ei, t),
      leaveout = isTRUE(leaveout),
      saveweights = isTRUE(saveweights),
      loadweights = !is.null(loadweights),
      saveestimates = isTRUE(saveestimates),
      saveresid = isTRUE(saveresid),
      horizons = horizons,
      allhorizons = isTRUE(allhorizons),
      hbalance = isTRUE(hbalance),
      pretrends = pretrends,
      pre_F = pretrend_contract$pre_F,
      pre_p = pretrend_contract$pre_p,
      pre_df = pretrend_contract$pre_df,
      significance_level = significance_level,
      ci_distribution = "normal",
      ci_degrees_of_freedom = Inf,
      ci_critical_value = z,
      shift = shift,
      delta = delta,
      wtr = wtr_cols,
      sum = isTRUE(sum_estimand),
      hetby = hetby,
      project = project,
      hbalance_included_units = weight_contract$hbalance_included_units,
      hbalance_excluded_units = weight_contract$hbalance_excluded_units,
      autosample = isTRUE(autosample),
      cannot_impute_row_ids = row_id[dt$.didbjs_cannot_impute],
      autosample_drop = autosample_drop,
      autosample_trim = autosample_trim,
      autosample_dropped_row_ids = if (isTRUE(autosample)) row_id[dt$.didbjs_cannot_impute] else character(),
      minn = minn,
      droplist = suppression_contract$droplist,
      suppressed_terms = suppression_contract$droplist,
      effective_n = suppression_contract$effective_n,
      suppression_adjusted_n_obs = sum(post_suppression_sample_mask)
    ),
    call = match.call()
  )
  if (isTRUE(saveweights)) {
    saved_weights <- reusable_saved_imputation_weights(
      covariance_weights = covariance_contract$imputation_weights,
      dt = analysis_dt,
      terms = weight_contract$terms,
      loadweights = loadweights_contract$matrix,
      controls = controls
    )
    if (is.null(saved_weights)) {
      saved_weights <- saved_imputation_weights(
        dt = analysis_dt,
        wtr_cols = weight_contract$columns,
        controls = controls,
        fe_cols = fe_cols
      )
    }
    result$artifacts$weights <- build_weight_artifact(
      raw_weights = saved_weights,
      row_id = analysis_row_id,
      terms = weight_contract$terms,
      metadata = weight_spec
    )
  }
  if (isTRUE(saveestimates)) {
    result$artifacts$estimates <- build_saved_estimates_artifact(
      dt = dt,
      y = y,
      row_id = row_id,
      sample_mask = sample_mask
    )
  }
  if (isTRUE(saveresid)) {
    result$artifacts$residuals <- build_residual_artifact(
      residuals = covariance_contract$residuals,
      row_id = analysis_row_id,
      terms = weight_contract$terms
    )
  }
  class(result) <- "didbjs"
  attr(result, "didbjs_object_version") <- "didbjs.result.v1"
  result
}

did_imputation_kyle <- function(data,
                                yname,
                                gname,
                                tname,
                                idname,
                                first_stage = NULL,
                                wname = NULL,
                                wtr = NULL,
                                horizon = NULL,
                                pretrends = NULL,
                                cluster_var = NULL,
                                subset = NULL) {
  horizons <- normalize_kyle_horizon(horizon)
  yvars <- parse_kyle_yname(yname)
  first_stage_contract <- kyle_first_stage_contract(first_stage, idname, tname)
  wtr_cols <- normalize_wtr(wtr)
  multi_outcome <- length(yvars) > 1

  dt <- data.table::as.data.table(data.table::copy(data))
  subset_mask <- normalize_subset_mask(subset, dt)
  dt <- data.table::copy(dt[subset_mask == TRUE])
  required <- c(yvars, gname, tname, idname, first_stage_contract$controls, fixed_effect_source_columns(first_stage_contract$fe), wtr_cols)
  missing_cols <- setdiff(required, names(dt))
  if (length(missing_cols) > 0) {
    stop_contract("Missing required Kyle columns: ", paste(missing_cols, collapse = ", "))
  }
  if (!is.null(wname) && !wname %in% names(dt)) {
    stop_contract("Missing Kyle weight column: ", wname)
  }
  if (!is.null(cluster_var) && !cluster_var %in% names(dt)) {
    stop_contract("Missing Kyle cluster column: ", cluster_var)
  }

  timing <- dt[[gname]]
  if (any(!is.na(timing) & is.finite(timing) & timing == 0)) {
    dt[[gname]][!is.na(timing) & is.finite(timing) & timing == 0] <- NA
  }

  out <- data.table::rbindlist(lapply(yvars, function(yvar) {
    result <- tryCatch(
      did_imputation(
        data = dt,
        y = yvar,
        i = idname,
        t = tname,
        Ei = gname,
        controls = first_stage_contract$controls,
        fe = first_stage_contract$fe,
        aw = wname,
        cluster = cluster_var %||% idname,
        minn = 0,
        wtr = wtr_cols,
        horizons = horizons
      ),
      didbjs_contract_error = function(err) {
        if (is.null(horizons) &&
            length(wtr_cols) == 0 &&
            is.null(pretrends) &&
            grepl("^Could not impute treated observations:", conditionMessage(err))) {
          return(kyle_static_missingness_fallback(dt, yvar, gname, tname, idname, wname))
        }
        stop(err)
      }
    )
    if (data.table::is.data.table(result)) {
      outcome_out <- result
      if (isTRUE(multi_outcome)) {
        outcome_out[, lhs := yvar]
        data.table::setcolorder(outcome_out, c("lhs", "term", "estimate", "std.error", "conf.low", "conf.high"))
      }
      return(outcome_out)
    }
    estimate <- result$estimates$estimate
    std_error <- result$estimates$std.error
    terms <- kyle_effect_terms(result$estimates$term, horizons = horizons, wtr = wtr_cols)
    outcome_out <- data.table::data.table(
      term = terms,
      estimate = estimate,
      std.error = std_error,
      conf.low = estimate - 1.96 * std_error,
      conf.high = estimate + 1.96 * std_error
    )
    pretrend_out <- kyle_pretrend_output(
      data = dt,
      yname = yvar,
      gname = gname,
      tname = tname,
      idname = idname,
      wname = wname,
      pretrends = pretrends,
      first_stage_rhs = first_stage_contract$rhs,
      cluster_var = cluster_var %||% idname
    )
    if (!is.null(pretrend_out)) {
      outcome_out <- data.table::rbindlist(list(pretrend_out, outcome_out), use.names = TRUE)
    }
    if (isTRUE(multi_outcome)) {
      outcome_out[, lhs := yvar]
      data.table::setcolorder(outcome_out, c("lhs", "term", "estimate", "std.error", "conf.low", "conf.high"))
    }
    outcome_out
  }), use.names = TRUE)
  if (!is.null(horizons)) {
    if (isTRUE(multi_outcome)) {
      out <- out[order(lhs, as.numeric(term))]
    } else {
      out <- out[order(as.numeric(term))]
    }
  }
  out
}

kyle_static_missingness_fallback <- function(dt, yvar, gname, tname, idname, wname) {
  tryCatch({
    local_dt <- data.table::copy(dt)
    treatment_time <- local_dt[[gname]]
    calendar_time <- local_dt[[tname]]
    local_dt[, .didbjs_kyle_treat := as.integer(
      !is.na(treatment_time) &
        is.finite(treatment_time) &
        treatment_time > 0 &
        calendar_time >= treatment_time
    )]
    local_dt[is.na(.didbjs_kyle_treat), .didbjs_kyle_treat := 0L]
    local_dt[, .didbjs_kyle_weight := if (is.null(wname)) 1 else as.numeric(local_dt[[wname]])]
    invalid_weight <- !is.na(local_dt$.didbjs_kyle_weight) &
      (!is.finite(local_dt$.didbjs_kyle_weight) | local_dt$.didbjs_kyle_weight <= 0)
    if (any(invalid_weight)) {
      stop_contract("Non-missing Kyle-compatible weights must be positive and finite.")
    }

    formula <- stats::as.formula(paste0(yvar, " ~ 0 | ", idname, " + ", tname))
    first_stage <- fixest::feols(
      formula,
      data = local_dt[.didbjs_kyle_treat == 0],
      weights = ~.didbjs_kyle_weight,
      warn = FALSE,
      notes = FALSE,
      fixef.rm = "none",
      fixef.tol = 1e-6,
      fixef.iter = 10000
    )
    local_dt[, .didbjs_adjustment := local_dt[[yvar]] - as.numeric(stats::predict(first_stage, newdata = local_dt))]
    local_dt <- local_dt[!is.na(.didbjs_adjustment)]
    local_dt[, .didbjs_kyle_weight := as.numeric(.didbjs_kyle_treat == 1) * .didbjs_kyle_weight]
    local_dt[is.na(.didbjs_kyle_weight), .didbjs_kyle_weight := 0]
    total_weight <- sum(local_dt$.didbjs_kyle_weight)
    if (!is.finite(total_weight) || total_weight <= 0) {
      stop_contract("Kyle-compatible missingness path has no treated weight after filtering.")
    }
    local_dt[, .didbjs_kyle_weight := .didbjs_kyle_weight / total_weight]
    estimate <- local_dt[.didbjs_kyle_treat == 1, sum(.didbjs_kyle_weight * .didbjs_adjustment)]
    std_error <- NA_real_
    warning(structure(
      list(message = "Kyle-compatible missingness fallback returns NA standard errors and confidence intervals."),
      class = c("didbjs_kyle_missingness_warning", "warning", "condition")
    ))
    data.table::data.table(
      term = "treat",
      estimate = estimate,
      std.error = std_error,
      conf.low = estimate - 1.96 * std_error,
      conf.high = estimate + 1.96 * std_error
    )
  }, error = function(err) {
    if (inherits(err, "didbjs_contract_error")) {
      stop(err)
    }
    stop_contract("Kyle-compatible missingness fallback failed: ", conditionMessage(err))
  })
}

did_imputation_python <- function(df,
                                  y,
                                  i,
                                  t,
                                  Ei,
                                  controls = character(),
                                  fe = character(),
                                  timecontrols = character(),
                                  aw = NULL,
                                  unitcontrols = character(),
                                  wtr = character(),
                                  sum = FALSE,
                                  horizons = character(),
                                  allhorizons = FALSE,
                                  hbalance = FALSE,
                                  hetby = character(),
                                  project = character(),
                                  minn = 30,
                                  saveweights = FALSE,
                                  shift = 0,
                                  pretrends = 0,
                                  cluster = "",
                                  avgeffectsby = character(),
                                  leaveoneout = FALSE,
                                  nose = FALSE,
                                  delta = NULL,
                                  subset = NULL) {
  if (length(timecontrols) > 0 || length(unitcontrols) > 0) {
    stop_unsupported("Python timecontrols and unitcontrols placeholders are not implemented upstream.")
  }
  if (isTRUE(sum)) {
    stop_unsupported("Python sum estimands are not implemented yet.")
  }
  if (length(wtr) > 0 && (length(horizons) > 0 || isTRUE(allhorizons) || isTRUE(hbalance))) {
    stop_contract("Python custom wtr cannot be combined with horizons, allhorizons, or hbalance.")
  }
  if (isTRUE(allhorizons) && length(horizons) > 0) {
    stop_contract("Options horizons and allhorizons cannot be combined.")
  }
  if (length(hetby) > 0 || length(project) > 0) {
    stop_unsupported("Python hetby and project placeholders are not implemented upstream.")
  }
  if (!identical(shift, 0)) {
    stop_unsupported("Python shift output is not implemented yet.")
  }
  if (length(avgeffectsby) > 0) {
    stop_unsupported("Python avgeffectsby output is not implemented yet.")
  }
  if (isTRUE(leaveoneout)) {
    stop_unsupported("Python leaveoneout is not implemented upstream.")
  }
  if (isTRUE(nose)) {
    stop_unsupported("Python nose output is not implemented yet.")
  }
  if (!is.null(delta)) {
    stop_unsupported("Python delta validation is not implemented yet.")
  }
  fe_arg <- if (is.null(fe)) {
    NULL
  } else if (length(fe) == 0) {
    c(i, t)
  } else {
    as.character(fe)
  }
  cluster_arg <- if (identical(cluster, "")) i else cluster
  horizons_arg <- if (length(horizons) == 0) NULL else horizons

  result <- did_imputation(
    data = df,
    y = y,
    i = i,
    t = t,
    Ei = Ei,
    controls = controls,
    fe = fe_arg,
    aw = aw,
    cluster = cluster_arg,
    subset = subset,
    minn = minn,
    autosample = !isTRUE(hbalance),
    wtr = wtr,
    sum = isTRUE(sum),
    horizons = horizons_arg,
    allhorizons = isTRUE(allhorizons),
    hbalance = isTRUE(hbalance),
    pretrends = pretrends,
    saveweights = isTRUE(saveweights)
  )
  if (is.null(horizons_arg) && !isTRUE(allhorizons) &&
      (length(result$diagnostics$autosample_drop) > 0 || length(result$diagnostics$autosample_trim) > 0)) {
    failed <- result$diagnostics$cannot_impute_row_ids
    suffix <- if (length(failed) > 0) paste0(": ", paste(failed, collapse = ", ")) else "."
    stop_contract("Python-compatible static ATT cannot be identified after autosample", suffix)
  }
  effect_rows <- !grepl("^pre[0-9]+$", result$estimates$term)
  effect_estimates <- result$estimates[effect_rows, , drop = FALSE]
  estimate_names <- if (length(wtr) > 0) {
    as.character(wtr)
  } else if (identical(effect_estimates$term, "tau")) {
    "tau_ate"
  } else {
    effect_estimates$term
  }
  python_suppressed <- effect_estimates$term %in% result$diagnostics$suppressed_terms
  python_estimates <- ifelse(python_suppressed, 0, effect_estimates$estimate)
  python_std_errors <- ifelse(python_suppressed, 0, effect_estimates$std.error)
  estimates <- stats::setNames(as.list(python_estimates), estimate_names)
  std_errors <- stats::setNames(as.list(python_std_errors), estimate_names)
  python_v <- if (nrow(result$covariance) > 1) {
    sum(diag(result$covariance))
  } else {
    as.numeric(result$covariance[1, 1])
  }
  controls_estimates <- if (nrow(result$controls) > 0) {
    stats::setNames(as.list(result$controls$estimate), result$controls$term)
  } else {
    NULL
  }
  controls_std_errors <- if (nrow(result$controls) > 0) {
    stats::setNames(as.list(result$controls$std.error), result$controls$term)
  } else {
    NULL
  }
  out <- list(
    pretrends_estimates = pretrend_output_list(result, "estimate"),
    pretrends_std_errors = pretrend_output_list(result, "std.error"),
    estimates = estimates,
    std_errors = std_errors,
    controls_estimates = controls_estimates,
    controls_std_errors = controls_std_errors,
    n_obs = if (any(python_suppressed)) result$diagnostics$suppression_adjusted_n_obs else result$estimates$n_obs[[1]],
    weights = if (isTRUE(saveweights)) python_weight_output(result, estimate_names, effect_estimates$term) else NULL,
    V = python_v
  )
  attr(out, "diagnostics") <- result$diagnostics
  class(out) <- c("DIDImputationOutput", "didbjs_python")
  out
}

first_stage_formula <- function(y, controls, fe_cols) {
  lhs <- formula_column(y)
  rhs <- if (length(controls) == 0) "1" else paste(vapply(controls, formula_column, character(1)), collapse = " + ")
  if (length(fe_cols) == 0) {
    stats::as.formula(paste0(lhs, " ~ ", rhs))
  } else if (length(controls) == 0) {
    stats::as.formula(paste0(lhs, " ~ 0 | ", paste(vapply(fe_cols, formula_column, character(1)), collapse = " + ")))
  } else {
    stats::as.formula(paste0(lhs, " ~ ", rhs, " | ", paste(vapply(fe_cols, formula_column, character(1)), collapse = " + ")))
  }
}

formula_column <- function(name) {
  paste0("`", gsub("`", "\\\\`", name, fixed = TRUE), "`")
}

estimate_pretrends <- function(dt, y, controls, fe_cols, cluster, pretrends) {
  if (identical(pretrends, 0L)) {
    return(list(
      terms = character(),
      estimates = numeric(),
      covariance = matrix(numeric(), nrow = 0, ncol = 0),
      cluster_scores = NULL,
      pre_F = NA_real_,
      pre_p = NA_real_,
      pre_df = NA_real_
    ))
  }

  pre_cols <- paste0(".didbjs_pre_", seq_len(pretrends))
  pre_terms <- paste0("pre", seq_len(pretrends))
  for (h in seq_len(pretrends)) {
    dt[, (pre_cols[[h]]) := as.numeric(!is.na(.didbjs_event_time) & .didbjs_event_time == -h)]
  }
  pre_data <- dt[.didbjs_untreated == TRUE]
  formula <- first_stage_formula(y, c(controls, pre_cols), fe_cols)
  pre_fit <- fixest::feols(
    formula,
    data = pre_data,
    weights = ~.didbjs_weight,
    cluster = stats::as.formula(paste0("~", cluster)),
    warn = FALSE,
    notes = FALSE,
    fixef.rm = "none",
    fixef.tol = 1e-10,
    fixef.iter = 100000
  )
  coefs <- stats::coef(pre_fit)
  estimates <- stats::setNames(rep(0, pretrends), pre_terms)
  coef_idx <- match(pre_cols, names(coefs))
  present <- !is.na(coef_idx)
  estimates[present] <- coefs[coef_idx[present]]

  z <- suppressMessages(fixest::sparse_model_matrix(pre_fit, data = pre_data, type = c("rhs", "fixef")))
  model_terms <- names(coefs)
  dof_adj <- control_dof_adjustment(
    dt = pre_data,
    z0 = z,
    z = z,
    cluster = cluster,
    controls = model_terms,
    fe_cols = fe_cols
  )
  preresid <- as.numeric(stats::residuals(pre_fit))
  present_cols <- pre_cols[present]
  all_clusters <- data.table::data.table(.didbjs_cluster = unique(dt[[cluster]]))
  score_dt <- data.table::data.table(.didbjs_cluster = pre_data[[cluster]])
  for (h in seq_len(pretrends)) {
    score_col <- pre_terms[[h]]
    if (!pre_cols[[h]] %in% present_cols) {
      score_dt[, (score_col) := 0]
      next
    }
    rhs_cols <- c(controls, setdiff(present_cols, pre_cols[[h]]))
    aux_fit <- fixest::feols(
      first_stage_formula(pre_cols[[h]], rhs_cols, fe_cols),
      data = pre_data,
      weights = ~.didbjs_weight,
      cluster = stats::as.formula(paste0("~", cluster)),
      warn = FALSE,
      notes = FALSE,
      fixef.rm = "none",
      fixef.tol = 1e-10,
      fixef.iter = 100000
    )
    preweight <- as.numeric(stats::residuals(aux_fit)) * pre_data$.didbjs_weight
    denom <- sum(preweight[pre_data[[pre_cols[[h]]]] == 1], na.rm = TRUE)
    if (!is.finite(denom) || abs(denom) <= .Machine$double.eps) {
      stop_contract("Could not compute pretrend covariance weights for ", pre_terms[[h]], ".")
    }
    score_dt[, (score_col) := preweight / denom * preresid * sqrt(dof_adj)]
  }
  cluster_scores <- score_dt[
    ,
    lapply(.SD, sum),
    by = .didbjs_cluster,
    .SDcols = pre_terms
  ]
  cluster_scores <- merge(all_clusters, cluster_scores, by = ".didbjs_cluster", all.x = TRUE, sort = FALSE)
  for (term in pre_terms) {
    data.table::set(cluster_scores, which(is.na(cluster_scores[[term]])), term, 0)
  }
  pre_score_matrix <- as.matrix(cluster_scores[, ..pre_terms])
  pre_vcov <- crossprod(pre_score_matrix)
  dimnames(pre_vcov) <- list(pre_terms, pre_terms)
  pre_df <- data.table::uniqueN(pre_data[[cluster]]) - 1
  test <- joint_pretrend_test(estimates, pre_vcov, pre_df)
  list(
    terms = pre_terms,
    estimates = estimates,
    covariance = pre_vcov,
    cluster_scores = cluster_scores,
    pre_F = test$F,
    pre_p = test$p,
    pre_df = test$df
  )
}

joint_pretrend_test <- function(estimates, covariance, df) {
  ok <- is.finite(estimates)
  if (!any(ok)) {
    return(list(F = NA_real_, p = NA_real_, df = df))
  }
  b <- matrix(estimates[ok], ncol = 1)
  v <- covariance[ok, ok, drop = FALSE]
  inv <- tryCatch(solve(v), error = function(...) NULL)
  if (is.null(inv)) {
    qr_v <- qr(v)
    rank <- qr_v$rank
    if (rank <= 0) {
      return(list(F = NA_real_, p = NA_real_, df = df))
    }
    keep <- qr_v$pivot[seq_len(rank)]
    b <- b[keep, , drop = FALSE]
    v <- v[keep, keep, drop = FALSE]
    inv <- tryCatch(solve(v), error = function(...) NULL)
    if (is.null(inv)) {
      return(list(F = NA_real_, p = NA_real_, df = df))
    }
  } else {
    rank <- length(b)
  }
  f_stat <- as.numeric(t(b) %*% inv %*% b / rank)
  p_value <- stats::pf(f_stat, df1 = rank, df2 = df, lower.tail = FALSE)
  list(F = f_stat, p = p_value, df = df)
}

combine_covariance_blocks <- function(...) {
  blocks <- list(...)
  terms <- unlist(lapply(blocks, rownames), use.names = FALSE)
  out <- matrix(0, nrow = length(terms), ncol = length(terms), dimnames = list(terms, terms))
  for (block in blocks) {
    block_terms <- rownames(block)
    out[block_terms, block_terms] <- block
  }
  out
}

pretrend_output_list <- function(result, column) {
  pre_rows <- grepl("^pre[0-9]+$", result$estimates$term)
  if (!any(pre_rows)) {
    return(NULL)
  }
  stats::setNames(as.list(result$estimates[[column]][pre_rows]), result$estimates$term[pre_rows])
}

python_weight_output <- function(result, output_names, terms) {
  artifact <- result$artifacts$weights
  if (is.null(artifact)) {
    return(NULL)
  }
  rows <- unique(artifact$weights$row_id)
  out <- data.frame(.didbjs_row = seq_along(rows))
  for (idx in seq_along(terms)) {
    term <- terms[[idx]]
    name <- output_names[[idx]]
    python_name <- if (identical(name, "tau_ate")) {
      "copywtr"
    } else if (startsWith(term, "tau") && grepl("^tau[0-9]+$", term)) {
      paste0("copywtr", sub("^tau", "", term))
    } else {
      paste0("copy", name)
    }
    term_weights <- artifact$weights[artifact$weights$term == term, , drop = FALSE]
    idx_match <- match(rows, term_weights$row_id)
    out[[python_name]] <- term_weights$weight[idx_match]
  }
  out$.didbjs_row <- NULL
  out
}

saved_weight_spec <- function(row_id,
                              sample_signature,
                              terms,
                              i,
                              t,
                              Ei,
                              controls,
                              fe,
                              aw,
                              cluster,
                              wtr,
                              sum,
                              hetby,
                              project,
                              horizons,
                              allhorizons,
                              hbalance,
                              avgeffectsby,
                              leaveout,
                              autosample,
                              minn,
                              shift,
                              delta) {
  spec <- list(
    schema_version = "didbjs.weights.v1",
    row_id = sort(as.character(row_id)),
    sample_signature = sample_signature,
    terms = as.character(terms),
    i = i,
    t = t,
    Ei = Ei,
    controls = as.character(controls),
    fe = as.character(fe),
    aw = aw %||% "",
    cluster = cluster,
    wtr = as.character(wtr),
    sum = isTRUE(sum),
    hetby = as.character(hetby),
    project = as.character(project),
    horizons = if (is.null(horizons)) integer() else as.integer(horizons),
    allhorizons = isTRUE(allhorizons),
    hbalance = isTRUE(hbalance),
    avgeffectsby = as.character(avgeffectsby),
    leaveout = isTRUE(leaveout),
    autosample = isTRUE(autosample),
    minn = minn,
    shift = shift,
    delta = delta
  )
  spec$spec_hash <- stable_hash_object(spec)
  spec
}

saved_weight_sample_signature <- function(dt, row_id, columns) {
  columns <- columns[!is.na(columns) & nzchar(columns)]
  columns <- unique(columns[columns %in% names(dt)])
  row_id <- as.character(row_id)
  order_idx <- order(row_id, seq_along(row_id), method = "radix")
  list(
    schema_version = "didbjs.sample_signature.v1",
    n_rows = length(row_id),
    columns = columns,
    hash = stable_hash_sample_signature(dt, row_id, columns, order_idx)
  )
}

stable_hash_sample_signature <- function(dt, row_id, columns, order_idx) {
  tmp <- tempfile("didbjs-sample-signature-")
  on.exit(unlink(tmp), add = TRUE)
  con <- file(tmp, open = "wb")
  on.exit(if (!is.null(con)) close(con), add = TRUE)
  writeLines(c("didbjs.sample_signature.v1", columns), con, useBytes = TRUE)
  chunk_size <- 10000L
  n <- length(order_idx)
  for (start in seq.int(1L, n, by = chunk_size)) {
    end <- min(start + chunk_size - 1L, n)
    idx <- order_idx[start:end]
    values <- lapply(columns, function(col) as.character(dt[[col]])[idx])
    lines <- do.call(paste, c(list(row_id[idx]), values, sep = "\r"))
    writeLines(lines, con, useBytes = TRUE)
  }
  close(con)
  con <- NULL
  unname(tools::md5sum(tmp))
}

stable_hash <- function(text) {
  tmp <- tempfile("didbjs-hash-")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(text, tmp, useBytes = TRUE)
  unname(tools::md5sum(tmp))
}

stable_hash_object <- function(object) {
  stable_hash(c(
    "didbjs.object_hash.v1",
    capture.output(dput(object))
  ))
}

build_weight_artifact <- function(raw_weights, row_id, terms, metadata) {
  if (!is.matrix(raw_weights) || nrow(raw_weights) != length(row_id) || ncol(raw_weights) != length(terms)) {
    stop_contract("Internal saved-weight dimensions do not match the current sample.")
  }
  dimnames(raw_weights) <- list(as.character(row_id), terms)
  weights <- data.frame(
    row_id = rep(as.character(row_id), each = length(terms)),
    term = rep(terms, times = length(row_id)),
    weight = as.vector(t(raw_weights)),
    stringsAsFactors = FALSE
  )
  sparse_weights <- weights[weights$weight != 0, , drop = FALSE]
  row.names(sparse_weights) <- NULL
  structure(
    list(
      schema_version = "didbjs.weights.v1",
      metadata = metadata,
      representations = c("dense", "sparse"),
      weights = weights,
      sparse_weights = sparse_weights
    ),
    class = "didbjs_weights"
  )
}

build_saved_estimates_artifact <- function(dt, y, row_id, sample_mask) {
  estimates <- rep(NA_real_, nrow(dt))
  treated_sample <- dt$.didbjs_treated == TRUE & sample_mask == TRUE
  estimates[treated_sample] <- dt[[y]][treated_sample] - dt$.didbjs_y_hat[treated_sample]
  structure(
    list(
      schema_version = "didbjs.estimates.v1",
      estimates = data.frame(
        row_id = as.character(row_id),
        estimate = estimates,
        stringsAsFactors = FALSE
      )
    ),
    class = "didbjs_estimates"
  )
}

build_residual_artifact <- function(residuals, row_id, terms) {
  if (!is.matrix(residuals) || nrow(residuals) != length(row_id) || ncol(residuals) != length(terms)) {
    stop_contract("Internal residual dimensions do not match the current sample.")
  }
  dimnames(residuals) <- list(as.character(row_id), terms)
  rows <- data.frame(
    row_id = rep(as.character(row_id), each = length(terms)),
    term = rep(terms, times = length(row_id)),
    residual = as.vector(t(residuals)),
    stringsAsFactors = FALSE
  )
  structure(
    list(
      schema_version = "didbjs.residuals.v1",
      residuals = rows
    ),
    class = "didbjs_residuals"
  )
}

prepare_loadweights <- function(loadweights, row_id, terms, expected_spec_hash) {
  if (is.null(loadweights)) {
    return(list(matrix = NULL))
  }
  manual_dataframe <- FALSE
  if (inherits(loadweights, "didbjs")) {
    loadweights <- loadweights$artifacts$weights
  }
  metadata <- NULL
  sparse_input <- FALSE
  if (is.list(loadweights) && !is.data.frame(loadweights) &&
      (!is.null(loadweights$weights) || !is.null(loadweights$sparse_weights))) {
    metadata <- loadweights$metadata
    if (!identical(loadweights$schema_version, "didbjs.weights.v1")) {
      stop_contract("Loaded weights use an unsupported schema version.")
    }
    if (!is.null(loadweights$weights)) {
      loadweights <- loadweights$weights
    } else {
      loadweights <- loadweights$sparse_weights
      sparse_input <- TRUE
    }
  } else if (is.data.frame(loadweights)) {
    manual_dataframe <- TRUE
  }
  if (!is.data.frame(loadweights)) {
    stop_contract("loadweights must be a didbjs weight artifact or a data frame with row_id, term, and weight.")
  }
  required <- c("row_id", "term", "weight")
  missing_cols <- setdiff(required, names(loadweights))
  if (length(missing_cols) > 0) {
    stop_contract("Loaded weights are missing columns: ", paste(missing_cols, collapse = ", "))
  }
  if (!is.null(metadata$spec_hash) && !identical(metadata$spec_hash, expected_spec_hash)) {
    stop_contract("Loaded weights were created for an incompatible sample or specification.")
  }
  if (isTRUE(manual_dataframe)) {
    warn_manual_loadweights()
  }
  loadweights$row_id <- as.character(loadweights$row_id)
  loadweights$term <- as.character(loadweights$term)
  if (anyDuplicated(paste(loadweights$row_id, loadweights$term, sep = "\r"))) {
    stop_contract("Loaded weights contain duplicate row_id/term pairs.")
  }
  unknown_rows <- setdiff(unique(loadweights$row_id), as.character(row_id))
  if (length(unknown_rows) > 0) {
    stop_contract("Loaded weights contain row_id values outside the current sample: ", paste(unknown_rows, collapse = ", "))
  }
  unknown_terms <- setdiff(unique(loadweights$term), terms)
  if (length(unknown_terms) > 0) {
    stop_contract("Loaded weights contain unknown terms: ", paste(unknown_terms, collapse = ", "))
  }
  mat <- matrix(NA_real_, nrow = length(row_id), ncol = length(terms), dimnames = list(as.character(row_id), terms))
  for (term in terms) {
    term_weights <- loadweights[loadweights$term == term, , drop = FALSE]
    idx <- match(as.character(row_id), term_weights$row_id)
    if (sparse_input) {
      values <- numeric(length(row_id))
      matched <- !is.na(idx)
      values[matched] <- term_weights$weight[idx[matched]]
    } else {
      if (anyNA(idx)) {
        stop_contract("Loaded weights do not contain every row required by the current sample.")
      }
      values <- term_weights$weight[idx]
    }
    if (anyNA(values) || any(!is.finite(values))) {
      stop_contract("Loaded weights must be finite for every required row.")
    }
    mat[, term] <- values
  }
  list(matrix = mat)
}

warn_manual_loadweights <- function() {
  warning_condition <- structure(
    list(message = "Bare data-frame loadweights are a manual override: didbjs cannot verify spec_hash metadata, but row_id/term/weight coverage and finite values are still checked."),
    class = c("didbjs_manual_loadweights_warning", "warning", "condition")
  )
  if (getOption("warn", 0) >= 2) {
    signalCondition(warning_condition)
  } else {
    warning(warning_condition)
  }
}

validate_unique_row_id <- function(row_id) {
  duplicate <- duplicated(row_id) | duplicated(row_id, fromLast = TRUE)
  if (any(duplicate)) {
    stop_contract("row_id values must uniquely identify observations; duplicate row ids: ", paste(unique(row_id[duplicate]), collapse = ", "))
  }
  invisible(TRUE)
}

saved_imputation_weights <- function(dt, wtr_cols, controls, fe_cols) {
  iterative_imputation_weights(
    dt = dt,
    wtr_cols = wtr_cols,
    controls = controls,
    fe_cols = fe_cols,
    tol = 1e-6,
    maxit = 1000
  )
}

reusable_saved_imputation_weights <- function(covariance_weights, dt, terms, loadweights, controls) {
  if (!is.null(loadweights)) {
    return(NULL)
  }
  if (length(controls) == 0) {
    return(NULL)
  }
  if (!is.matrix(covariance_weights) ||
      nrow(covariance_weights) != nrow(dt) ||
      ncol(covariance_weights) != length(terms)) {
    return(NULL)
  }
  if (any(!is.finite(dt$.didbjs_weight)) || any(dt$.didbjs_weight != 1)) {
    return(NULL)
  }
  dimnames(covariance_weights) <- list(NULL, terms)
  covariance_weights
}

prepare_fixed_effects <- function(dt, fe_terms) {
  if (length(fe_terms) == 0) {
    return(list(terms = character(), cols = character()))
  }
  if (anyNA(fe_terms) || any(fe_terms == "")) {
    stop_contract("Fixed-effect names cannot be missing or empty.")
  }

  fe_cols <- character(length(fe_terms))
  for (idx in seq_along(fe_terms)) {
    term <- fe_terms[[idx]]
    if (grepl("(^#|#$|##)", term)) {
      stop_contract("Invalid fixed-effect interaction term: ", term)
    }
    parts <- strsplit(term, "#", fixed = TRUE)[[1]]
    if (length(parts) == 0 || any(parts == "")) {
      stop_contract("Invalid fixed-effect interaction term: ", term)
    }
    missing_parts <- setdiff(parts, names(dt))
    if (length(missing_parts) > 0) {
      stop_contract("Missing fixed-effect columns: ", paste(missing_parts, collapse = ", "))
    }
    if (length(parts) == 1) {
      fe_cols[[idx]] <- parts[[1]]
    } else {
      fe_col <- internal_fe_column_name(names(dt), idx)
      interaction_args <- c(lapply(parts, function(part) dt[[part]]), list(drop = TRUE, lex.order = TRUE, sep = "#"))
      dt[, (fe_col) := do.call(interaction, interaction_args)]
      fe_cols[[idx]] <- fe_col
    }
  }
  list(terms = fe_terms, cols = fe_cols)
}

internal_fe_column_name <- function(existing_names, idx) {
  candidate <- paste0(".didbjs_fe_", idx)
  while (candidate %in% existing_names) {
    idx <- idx + 1L
    candidate <- paste0(".didbjs_fe_", idx)
  }
  candidate
}

treatment_event_time <- function(calendar_time, treatment_time, shift, delta) {
  event_time <- rep(NA_real_, length(calendar_time))
  timed <- !is.na(treatment_time) & is.finite(treatment_time)
  event_time[timed] <- (calendar_time[timed] - treatment_time[timed] + shift) / delta
  event_time
}

validate_treatment_timing <- function(dt, i, t, Ei) {
  if (!is.numeric(dt[[t]])) {
    stop_contract("Time column must be numeric.")
  }
  if (!is.numeric(dt[[Ei]])) {
    stop_contract("Treatment timing column must be numeric.")
  }
  timing <- dt[[Ei]]
  timing_state <- ifelse(is.na(timing) | !is.finite(timing), "never", paste0("finite:", timing))
  check <- data.table::data.table(.unit = dt[[i]], .timing_state = timing_state)
  inconsistent <- check[, .(n_timing_states = data.table::uniqueN(.timing_state)), by = .unit][n_timing_states > 1]
  if (nrow(inconsistent) > 0) {
    bad_units <- paste(inconsistent$.unit, collapse = ", ")
    stop_contract("Treatment timing must be constant within unit; inconsistent units: ", bad_units)
  }
  invisible(TRUE)
}

validate_unique_unit_time <- function(dt, row_id, i, t) {
  keys <- paste(as.character(dt[[i]]), as.character(dt[[t]]), sep = "\r")
  duplicate <- duplicated(keys) | duplicated(keys, fromLast = TRUE)
  if (any(duplicate)) {
    bad <- paste(row_id[duplicate], collapse = ", ")
    stop_contract("Duplicate unit-time rows are not allowed; duplicate row ids: ", bad)
  }
  invisible(TRUE)
}

treatment_rows_with_nonzero_weights <- function(dt, columns) {
  weights <- as.matrix(dt[, ..columns])
  rowSums(!is.na(weights) & weights != 0) > 0
}

apply_autosample <- function(dt, terms, columns) {
  drop_terms <- character()
  trim_terms <- character()

  for (idx in seq_along(columns)) {
    col <- columns[[idx]]
    term <- terms[[idx]]
    affected <- dt$.didbjs_cannot_impute & dt$.didbjs_treated & !is.na(dt[[col]]) & dt[[col]] != 0
    if (!any(affected)) {
      next
    }

    remaining <- dt$.didbjs_treated & !dt$.didbjs_cannot_impute & !is.na(dt[[col]]) & dt[[col]] != 0
    if (!any(remaining)) {
      dt[.didbjs_treated == TRUE, (col) := 0]
      drop_terms <- c(drop_terms, term)
      next
    }

    remaining_sum <- sum(dt[[col]][remaining])
    if (!is.finite(remaining_sum) || abs(remaining_sum) <= .Machine$double.eps) {
      dt[.didbjs_treated == TRUE, (col) := 0]
      drop_terms <- c(drop_terms, term)
      next
    }
    dt[remaining, (col) := get(col) / remaining_sum]
    dt[affected, (col) := 0]
    trim_terms <- c(trim_terms, term)
  }

  list(data = dt, drop_terms = drop_terms, trim_terms = trim_terms)
}

apply_minn_suppression <- function(dt, terms, columns, sample_mask, minn) {
  effective_n <- stats::setNames(rep(Inf, length(columns)), terms)
  droplist <- character()

  if (minn == 0L) {
    return(list(data = dt, droplist = droplist, effective_n = effective_n))
  }

  for (idx in seq_along(columns)) {
    col <- columns[[idx]]
    term <- terms[[idx]]
    treated <- sample_mask & dt$.didbjs_treated & !is.na(dt[[col]])
    abs_weights <- abs(dt[[col]][treated])
    total <- sum(abs_weights)
    if (!is.finite(total) || total <= 0) {
      dt[sample_mask == TRUE, (col) := 0]
      effective_n[[term]] <- 0
      droplist <- c(droplist, term)
      next
    }
    hhi <- sum((abs_weights / total)^2)
    effective_n[[term]] <- if (hhi > 0) 1 / hhi else Inf
    if (hhi > 1 / minn) {
      dt[sample_mask == TRUE, (col) := 0]
      droplist <- c(droplist, term)
    }
  }

  list(data = dt, droplist = droplist, effective_n = effective_n)
}

normalize_integer_scalar <- function(value, name) {
  if (!is.numeric(value) || length(value) != 1 || is.na(value) || !is.finite(value) || value != as.integer(value)) {
    stop_contract(name, " must be a finite integer scalar.")
  }
  as.integer(value)
}

normalize_nonnegative_integer_scalar <- function(value, name) {
  value <- normalize_integer_scalar(value, name)
  if (value < 0) {
    stop_contract(name, " must be non-negative.")
  }
  value
}

normalize_delta <- function(delta) {
  if (is.null(delta) || identical(delta, 0) || identical(delta, 0L)) {
    return(1L)
  }
  value <- normalize_integer_scalar(delta, "delta")
  if (value <= 0) {
    stop_contract("delta must be positive.")
  }
  value
}

normalize_significance_level <- function(significance_level) {
  if (!is.numeric(significance_level) ||
      length(significance_level) != 1 ||
      is.na(significance_level) ||
      !is.finite(significance_level) ||
      significance_level <= 0 ||
      significance_level >= 1) {
    stop_contract("significance_level must be a finite number between 0 and 1.")
  }
  as.numeric(significance_level)
}

normalize_horizons <- function(horizons) {
  if (is.null(horizons)) {
    return(NULL)
  }
  if (length(horizons) == 0) {
    stop_contract("Empty horizons are invalid. Use NULL for static ATT.")
  }
  if (anyNA(horizons)) {
    stop_contract("Horizons cannot contain missing values.")
  }
  if (any(!is.finite(horizons))) {
    stop_contract("Horizons must be finite non-negative integers.")
  }
  if (any(horizons < 0)) {
    stop_contract("Horizons must be non-negative.")
  }
  if (any(horizons != as.integer(horizons))) {
    stop_contract("Horizons must be integers.")
  }
  horizons <- as.integer(horizons)
  if (anyDuplicated(horizons)) {
    stop_contract("Horizons cannot contain duplicates.")
  }
  horizons
}

normalize_allhorizons <- function(allhorizons) {
  normalize_flag(allhorizons, "allhorizons")
}

normalize_subset_mask <- function(subset, dt) {
  if (is.null(subset)) {
    return(rep(TRUE, nrow(dt)))
  }
  if (is.character(subset) && length(subset) == 1) {
    if (!subset %in% names(dt)) {
      stop_contract("subset column not found: ", subset)
    }
    subset <- dt[[subset]]
  }
  if (is.numeric(subset) && length(subset) == nrow(dt)) {
    if (anyNA(subset) || any(!is.finite(subset)) || any(!subset %in% c(0, 1))) {
      stop_contract("numeric subset values must be finite 0/1 indicators.")
    }
    subset <- subset == 1
  }
  if (!is.logical(subset) || length(subset) != nrow(dt)) {
    stop_contract("subset must be NULL, a logical or 0/1 vector with one value per row, or a single subset column name.")
  }
  subset[is.na(subset)] <- FALSE
  subset
}

normalize_flag <- function(value, name) {
  if (!identical(value, TRUE) && !identical(value, FALSE)) {
    stop_contract(name, " must be TRUE or FALSE.")
  }
  value
}

normalize_wtr <- function(wtr) {
  if (is.null(wtr) || length(wtr) == 0) {
    return(character())
  }
  if (anyNA(wtr)) {
    stop_contract("Custom wtr names cannot be missing.")
  }
  wtr <- as.character(wtr)
  if (any(wtr == "")) {
    stop_contract("Custom wtr names cannot be empty.")
  }
  if (anyDuplicated(wtr)) {
    stop_contract("Custom wtr names cannot contain duplicates.")
  }
  wtr
}

normalize_controls <- function(controls) {
  if (is.null(controls) || length(controls) == 0) {
    return(character())
  }
  if (anyNA(controls)) {
    stop_contract("Control names cannot be missing.")
  }
  controls <- as.character(controls)
  if (any(controls == "")) {
    stop_contract("Control names cannot be empty.")
  }
  if (anyDuplicated(controls)) {
    stop_contract("Control names cannot contain duplicates.")
  }
  controls
}

normalize_single_column <- function(value, name) {
  if (is.null(value) || length(value) == 0) {
    return(NULL)
  }
  if (length(value) != 1 || is.na(value) || identical(value, "")) {
    stop_contract(name, " must be a single non-empty column name.")
  }
  as.character(value)
}

normalize_avgeffectsby <- function(avgeffectsby) {
  if (is.null(avgeffectsby) || length(avgeffectsby) == 0) {
    return(NULL)
  }
  if (anyNA(avgeffectsby)) {
    stop_contract("avgeffectsby names cannot be missing.")
  }
  avgeffectsby <- as.character(avgeffectsby)
  if (any(avgeffectsby == "")) {
    stop_contract("avgeffectsby names cannot be empty.")
  }
  if (anyDuplicated(avgeffectsby)) {
    stop_contract("avgeffectsby names cannot contain duplicates.")
  }
  avgeffectsby
}

fixed_effect_source_columns <- function(fe_terms) {
  if (length(fe_terms) == 0) {
    return(character())
  }
  unique(unlist(strsplit(fe_terms, "#", fixed = TRUE), use.names = FALSE))
}

first_stage_control_coefficients <- function(first_stage, controls) {
  if (length(controls) == 0) {
    return(numeric())
  }
  coefs <- stats::coef(first_stage)
  missing_controls <- setdiff(controls, names(coefs))
  if (length(missing_controls) > 0 || any(!is.finite(coefs[controls]))) {
    stop_contract(
      "Could not run imputation for some observations because some controls are collinear in the D==0 subsample but not in the full sample"
    )
  }
  stats::setNames(as.numeric(coefs[controls]), controls)
}

normalize_kyle_horizon <- function(horizon) {
  if (is.null(horizon)) {
    return(NULL)
  }
  if (identical(horizon, TRUE)) {
    stop_unsupported("Kyle horizon = TRUE all-horizon output is not implemented yet.")
  }
  normalize_horizons(horizon)
}

kyle_first_stage_contract <- function(first_stage, idname, tname) {
  rhs <- if (is.null(first_stage)) {
    paste0("0 | ", idname, " + ", tname)
  } else if (inherits(first_stage, "formula")) {
    pieces <- as.character(first_stage)
    if (length(pieces) != 2) {
      stop_unsupported("Kyle first_stage formulas must be one-sided formulas in didbjs.")
    }
    pieces[[2]]
  } else if (is.character(first_stage) && length(first_stage) == 1 && !is.na(first_stage) && nzchar(first_stage)) {
    first_stage
  } else {
    stop_contract("Kyle first_stage must be NULL, a one-sided formula, or a single formula string.")
  }

  pieces <- strsplit(rhs, "|", fixed = TRUE)[[1]]
  if (length(pieces) > 2) {
    stop_unsupported("Kyle first_stage formulas with multiple | separators are not supported.")
  }
  controls <- kyle_first_stage_terms(pieces[[1]], "control")
  fe <- if (length(pieces) == 2) kyle_first_stage_terms(pieces[[2]], "fixed-effect") else character()
  list(rhs = rhs, controls = controls, fe = fe)
}

kyle_first_stage_terms <- function(text, label) {
  text <- trimws(text)
  if (!nzchar(text) || text %in% c("0", "1")) {
    return(character())
  }
  if (grepl("[:*^()]", text)) {
    stop_unsupported("Kyle first_stage ", label, " formulas with transformations or interactions are not supported.")
  }
  terms <- all.vars(stats::as.formula(paste0("~", text)))
  if (length(terms) == 0) {
    return(character())
  }
  if (anyDuplicated(terms)) {
    stop_contract("Kyle first_stage ", label, " terms cannot contain duplicates.")
  }
  terms
}

kyle_effect_terms <- function(terms, horizons, wtr) {
  if (length(wtr) > 0) {
    return(wtr)
  }
  if (is.null(horizons)) {
    return("treat")
  }
  sub("^tau", "", terms)
}

parse_kyle_yname <- function(yname) {
  if (length(yname) != 1 || is.na(yname) || identical(yname, "")) {
    stop_contract("Kyle yname must be a single non-empty string.")
  }
  yname <- trimws(as.character(yname))
  match <- regexec("^c\\((.*)\\)$", yname)
  pieces <- regmatches(yname, match)[[1]]
  if (length(pieces) == 0) {
    return(yname)
  }
  yvars <- trimws(strsplit(pieces[[2]], ",", fixed = TRUE)[[1]])
  if (length(yvars) == 0 || any(yvars == "") || anyNA(yvars)) {
    stop_contract("Kyle multi-outcome yname must contain non-empty outcome names.")
  }
  if (anyDuplicated(yvars)) {
    stop_contract("Kyle multi-outcome yname cannot contain duplicate outcomes.")
  }
  yvars
}

kyle_pretrend_output <- function(data, yname, gname, tname, idname, wname, pretrends, first_stage_rhs, cluster_var) {
  if (is.null(pretrends) || all(pretrends == FALSE)) {
    return(NULL)
  }
  dt <- data.table::as.data.table(data.table::copy(data))
  dt[, .didbjs_kyle_treat := as.numeric(dt[[tname]] >= dt[[gname]] & dt[[gname]] > 0)]
  dt[is.na(.didbjs_kyle_treat), .didbjs_kyle_treat := 0]
  dt[, .didbjs_kyle_event_time := ifelse(
    is.na(dt[[gname]]) | dt[[gname]] == 0 | dt[[gname]] == Inf,
    -Inf,
    as.numeric(dt[[tname]] - dt[[gname]])
  )]
  dt[, .didbjs_kyle_weight := if (is.null(wname)) 1 else dt[[wname]]]
  event_time <- dt[is.finite(.didbjs_kyle_event_time), unique(.didbjs_kyle_event_time)]

  if (all(pretrends == TRUE)) {
    rhs <- paste0("i(.didbjs_kyle_event_time) + ", first_stage_rhs)
  } else {
    if (anyNA(pretrends) || any(!is.finite(pretrends)) || any(pretrends != as.integer(pretrends))) {
      stop_contract("Kyle pretrends must be TRUE, FALSE, NULL, or finite integer event times.")
    }
    pretrends <- as.integer(pretrends)
    if (!all(pretrends %in% event_time)) {
      stop_contract(
        "Pretrends not found in event_time. Event_time has values",
        paste(event_time, collapse = "")
      )
    }
    rhs <- paste0(
      "i(.didbjs_kyle_event_time, keep = c(",
      paste(pretrends, collapse = ", "),
      ")) + ",
      first_stage_rhs
    )
  }
  pre_fit <- fixest::feols(
    stats::as.formula(paste0(yname, " ~ ", rhs)),
    data = dt[.didbjs_kyle_treat == 0],
    cluster = stats::as.formula(paste0("~", cluster_var)),
    weights = ~.didbjs_kyle_weight,
    warn = FALSE,
    notes = FALSE,
    fixef.rm = "none"
  )
  pre_out <- data.table::as.data.table(pre_fit$coeftable, keep.rownames = "term")
  if (nrow(pre_out) == 0) {
    return(NULL)
  }
  data.table::setnames(pre_out, c("term", "estimate", "std.error", "t_value", "p_value"))
  pre_out <- pre_out[grep(".didbjs_kyle_event_time::", term, fixed = TRUE)]
  if (nrow(pre_out) == 0) {
    return(NULL)
  }
  pre_out[, term := sub("^\\.didbjs_kyle_event_time::", "", term)]
  pre_out[, conf.low := estimate - 1.96 * std.error]
  pre_out[, conf.high := estimate + 1.96 * std.error]
  pre_out[, c("term", "estimate", "std.error", "conf.low", "conf.high")]
}

build_treatment_weights <- function(dt,
                                    horizons,
                                    hbalance = FALSE,
                                    unit_col = NULL,
                                    custom_wtr = character(),
                                    sum_estimand = FALSE,
                                    hetby = NULL,
                                    project = character()) {
  if (length(custom_wtr) > 0) {
    if (!is.null(horizons) || isTRUE(hbalance)) {
      stop_contract("Custom wtr cannot be combined with horizon weights in the current conformance path.")
    }
    terms <- if (length(custom_wtr) == 1) "tau" else paste0("tau_", custom_wtr)
    columns <- paste0(".didbjs_wtr_custom_", seq_along(custom_wtr))
    n_treated <- integer(length(custom_wtr))
    for (idx in seq_along(custom_wtr)) {
      source_col <- custom_wtr[[idx]]
      target_col <- columns[[idx]]
      values <- dt[[source_col]]
      if (!is.numeric(values)) {
        stop_contract("Custom wtr column must be numeric: ", source_col)
      }
      if (anyNA(values[dt$.didbjs_treated])) {
        stop_contract("Custom wtr column has missing values on treated observations: ", source_col)
      }
      if (any(!is.finite(values[dt$.didbjs_treated]))) {
        stop_contract("Custom wtr column has non-finite values on treated observations: ", source_col)
      }
      if (!isTRUE(sum_estimand) && any(values < 0, na.rm = TRUE)) {
        stop_contract("Negative custom wtr values require sum = TRUE.")
      }
      raw_weight <- numeric(nrow(dt))
      raw_weight[dt$.didbjs_treated] <- values[dt$.didbjs_treated] * dt$.didbjs_weight[dt$.didbjs_treated]
      if (isTRUE(sum_estimand)) {
        if (!is.finite(sum(abs(raw_weight))) || sum(abs(raw_weight)) <= 0) {
          stop_contract("Custom sum wtr column has zero absolute treated weight after analytic weights: ", source_col)
        }
        dt[, (target_col) := raw_weight]
      } else {
        total <- sum(raw_weight)
        if (!is.finite(total) || total < 0) {
          stop_contract("Custom wtr column has zero treated weight after analytic weights: ", source_col)
        }
        if (total == 0) {
          dt[, (target_col) := 0]
        } else {
          dt[, (target_col) := raw_weight / total]
        }
      }
      n_treated[[idx]] <- sum(dt$.didbjs_treated & values != 0)
    }
    out <- list(
      data = dt,
      terms = terms,
      columns = columns,
      n_treated = n_treated,
      hbalance_included_units = NULL,
      hbalance_excluded_units = NULL
    )
    return(apply_heterogeneity_or_projection(out, hetby = hetby, project = project, sum_estimand = sum_estimand))
  }

  if (is.null(horizons)) {
    dt[, .didbjs_wtr_tau := as.numeric(.didbjs_treated) * .didbjs_weight]
    total <- sum(dt$.didbjs_wtr_tau)
    if (!is.finite(total) || total <= 0) {
      stop_contract("Static ATT has zero treated weight.")
    }
    dt[, .didbjs_wtr_tau := .didbjs_wtr_tau / total]
    out <- list(
      data = dt,
      terms = "tau",
      columns = ".didbjs_wtr_tau",
      n_treated = sum(dt$.didbjs_treated),
      hbalance_included_units = NULL,
      hbalance_excluded_units = NULL
    )
    return(apply_heterogeneity_or_projection(out, hetby = hetby, project = project, sum_estimand = sum_estimand))
  }

  hbalance_contract <- hbalance_units(dt, horizons, hbalance, unit_col)
  included_units <- hbalance_contract$included_units
  terms <- paste0("tau", horizons)
  columns <- paste0(".didbjs_wtr_", terms)
  n_treated <- integer(length(horizons))
  for (idx in seq_along(horizons)) {
    horizon <- horizons[[idx]]
    col <- columns[[idx]]
    include <- dt$.didbjs_treated & dt$.didbjs_event_time == horizon
    if (isTRUE(hbalance)) {
      include <- include & dt[[unit_col]] %in% included_units
    }
    dt[, (col) := as.numeric(include) * .didbjs_weight]
    total <- sum(dt[[col]])
    if (!is.finite(total) || total <= 0) {
      stop_contract("Horizon ", horizon, " has zero treated weight.")
    }
    dt[, (col) := get(col) / total]
    n_treated[[idx]] <- sum(include)
  }
  out <- list(
    data = dt,
    terms = terms,
    columns = columns,
    n_treated = n_treated,
    hbalance_included_units = hbalance_contract$included_units,
    hbalance_excluded_units = hbalance_contract$excluded_units
  )
  apply_heterogeneity_or_projection(out, hetby = hetby, project = project, sum_estimand = sum_estimand)
}

apply_heterogeneity_or_projection <- function(weight_contract, hetby, project, sum_estimand) {
  if (!is.null(hetby)) {
    return(apply_hetby_weights(weight_contract, hetby = hetby, sum_estimand = sum_estimand))
  }
  if (length(project) > 0) {
    return(apply_project_weights(weight_contract, project = project))
  }
  weight_contract
}

apply_hetby_weights <- function(weight_contract, hetby, sum_estimand) {
  dt <- weight_contract$data
  validate_hetby_values(dt, hetby)
  raw_levels <- sort(unique(dt[.didbjs_treated == TRUE & !is.na(get(hetby)), get(hetby)]))
  if (length(raw_levels) == 0) {
    stop_contract("The hetby variable is always missing.")
  }
  if (length(raw_levels) > 30) {
    stop_contract("The hetby variable takes too many (over 30) values.")
  }

  new_terms <- character()
  new_columns <- character()
  new_n_treated <- integer()
  for (idx in seq_along(weight_contract$columns)) {
    source_col <- weight_contract$columns[[idx]]
    source_term <- weight_contract$terms[[idx]]
    for (level in raw_levels) {
      level_label <- format_hetby_level(level)
      target_col <- paste0(source_col, "_hetby_", make.names(level_label))
      include <- dt$.didbjs_treated == TRUE & !is.na(dt[[hetby]]) & dt[[hetby]] == level
      dt[, (target_col) := ifelse(include, get(source_col), 0)]
      if (!isTRUE(sum_estimand)) {
        total <- sum(dt[[target_col]], na.rm = TRUE)
        if (!is.finite(total) || total <= 0) {
          stop_contract("Heterogeneity group ", level_label, " has zero treated weight.")
        }
        dt[, (target_col) := get(target_col) / total]
      }
      new_terms <- c(new_terms, paste0(source_term, "_", level_label))
      new_columns <- c(new_columns, target_col)
      new_n_treated <- c(new_n_treated, sum(include & dt[[source_col]] != 0, na.rm = TRUE))
    }
  }
  weight_contract$data <- dt
  weight_contract$terms <- new_terms
  weight_contract$columns <- new_columns
  weight_contract$n_treated <- new_n_treated
  weight_contract
}

validate_hetby_values <- function(dt, hetby) {
  values <- dt[.didbjs_treated == TRUE, get(hetby)]
  if (is.numeric(values)) {
    if (any(values < 0, na.rm = TRUE)) {
      stop_contract("The hetby variable cannot take negative values.")
    }
    finite_values <- values[!is.na(values) & is.finite(values)]
    if (any(finite_values != as.integer(finite_values))) {
      stop_contract("The hetby variable cannot take non-integer values.")
    }
  }
}

format_hetby_level <- function(level) {
  if (is.numeric(level) && is.finite(level) && level == as.integer(level)) {
    as.character(as.integer(level))
  } else {
    as.character(level)
  }
}

apply_project_weights <- function(weight_contract, project) {
  dt <- weight_contract$data
  new_terms <- character()
  new_columns <- character()
  new_n_treated <- integer()
  for (idx in seq_along(weight_contract$columns)) {
    source_col <- weight_contract$columns[[idx]]
    source_term <- weight_contract$terms[[idx]]
    support <- dt$.didbjs_treated == TRUE & !is.na(dt[[source_col]]) & dt[[source_col]] > 0
    if (!any(support)) {
      next
    }

    cons_resid <- weighted_residual(
      y = rep(1, sum(support)),
      x = as.matrix(dt[support, ..project]),
      weights = dt$.didbjs_weight[support],
      intercept = FALSE
    )
    cons_resid[abs(cons_resid) < 1e-12] <- 0
    cons_denom <- sum(dt$.didbjs_weight[support] * cons_resid^2)
    if (is.finite(cons_denom) && cons_denom >= 1e-6) {
      target_col <- paste0(source_col, "_project_cons")
      dt[, (target_col) := 0]
      dt[support == TRUE, (target_col) := dt$.didbjs_weight[support] * cons_resid / cons_denom]
      new_terms <- c(new_terms, paste0(source_term, "_cons"))
      new_columns <- c(new_columns, target_col)
      new_n_treated <- c(new_n_treated, sum(dt[[target_col]][support] != 0, na.rm = TRUE))
    }

    for (project_col in project) {
      other_project <- setdiff(project, project_col)
      x <- if (length(other_project) == 0) {
        matrix(numeric(), nrow = sum(support), ncol = 0)
      } else {
        as.matrix(dt[support, ..other_project])
      }
      resid <- weighted_residual(
        y = dt[[project_col]][support],
        x = x,
        weights = dt$.didbjs_weight[support],
        intercept = TRUE
      )
      resid[abs(resid) < 1e-12] <- 0
      denom <- sum(dt$.didbjs_weight[support] * resid^2)
      if (!is.finite(denom) || denom < 1e-6) {
        next
      }
      target_col <- paste0(source_col, "_project_", project_col)
      dt[, (target_col) := 0]
      dt[support == TRUE, (target_col) := dt$.didbjs_weight[support] * resid / denom]
      new_terms <- c(new_terms, paste0(source_term, "_", project_col))
      new_columns <- c(new_columns, target_col)
      new_n_treated <- c(new_n_treated, sum(dt[[target_col]][support] != 0, na.rm = TRUE))
    }
  }
  if (length(new_columns) == 0) {
    stop_contract("Projection is not possible, most likely because of collinearity.")
  }
  weight_contract$data <- dt
  weight_contract$terms <- new_terms
  weight_contract$columns <- new_columns
  weight_contract$n_treated <- new_n_treated
  weight_contract
}

weighted_residual <- function(y, x, weights, intercept) {
  design <- if (is.null(x) || ncol(x) == 0) {
    if (isTRUE(intercept)) matrix(1, nrow = length(y), ncol = 1) else matrix(numeric(), nrow = length(y), ncol = 0)
  } else if (isTRUE(intercept)) {
    cbind("(Intercept)" = 1, x)
  } else {
    x
  }
  if (ncol(design) == 0) {
    return(as.numeric(y))
  }
  fit <- stats::lm.wfit(x = design, y = as.numeric(y), w = as.numeric(weights))
  as.numeric(fit$residuals)
}

hbalance_units <- function(dt, horizons, hbalance, unit_col) {
  if (!isTRUE(hbalance)) {
    return(list(included_units = NULL, excluded_units = NULL))
  }
  if (is.null(unit_col) || !unit_col %in% names(dt)) {
    stop_contract("hbalance requires a valid unit column.")
  }
  in_requested <- dt$.didbjs_treated & !is.na(dt$.didbjs_event_time) & dt$.didbjs_event_time %in% horizons
  balance <- dt[
    in_requested == TRUE,
    .(
      .didbjs_requested_horizon_count = .N,
      .didbjs_min_weight = min(.didbjs_weight),
      .didbjs_max_weight = max(.didbjs_weight)
    ),
    by = unit_col
  ]
  if (nrow(balance) == 0) {
    stop_contract("hbalance found no units with requested horizons.")
  }
  included <- balance[
    .didbjs_requested_horizon_count == length(horizons),
    get(unit_col)
  ]
  if (length(included) == 0) {
    stop_contract("hbalance found no units available for all requested horizons.")
  }
  bad_weights <- balance[
    .didbjs_requested_horizon_count == length(horizons) &
      .didbjs_max_weight > 1.000001 * .didbjs_min_weight
  ]
  if (nrow(bad_weights) > 0) {
    stop_contract("Weights must be identical across periods for units in the balanced sample.")
  }
  treated_units <- unique(dt[.didbjs_treated == TRUE, get(unit_col)])
  list(
    included_units = sort(included),
    excluded_units = sort(setdiff(treated_units, included))
  )
}

discover_all_horizons <- function(dt) {
  horizons <- sort(unique(dt[.didbjs_treated == TRUE & !is.na(.didbjs_event_time) & .didbjs_event_time >= 0, .didbjs_event_time]))
  if (length(horizons) == 0) {
    stop_contract("No non-negative treated horizons are available.")
  }
  if (any(horizons != as.integer(horizons))) {
    stop_contract("Discovered horizons must be integers.")
  }
  as.integer(horizons)
}

static_cluster_covariance <- function(dt,
                                      first_stage,
                                      cluster,
                                      Ei,
                                      terms,
                                      wtr_cols,
                                      controls = character(),
                                      fe_cols = character(),
                                      avgeffectsby = NULL,
                                      leaveout = FALSE,
                                      loadweights = NULL,
                                      extra_cluster_scores = NULL,
                                      extra_terms = character()) {
  if (length(unique(dt[[cluster]])) < 2) {
    stop_contract("Clustered covariance requires at least two clusters.")
  }

  z <- suppressMessages(fixest::sparse_model_matrix(first_stage, data = dt, type = c("rhs", "fixef")))
  if (nrow(z) != nrow(dt)) {
    if (ncol(z) == 1 && identical(colnames(z), "(Intercept)")) {
      z <- Matrix::Matrix(
        1,
        nrow = nrow(dt),
        ncol = 1,
        dimnames = list(NULL, "(Intercept)"),
        sparse = TRUE
      )
    } else {
      stop_contract("Could not construct a full first-stage design matrix for covariance estimation.")
    }
  }
  unit_weights <- all(dt$.didbjs_weight == 1)
  z_weighted <- if (isTRUE(unit_weights)) z else Matrix::Diagonal(x = dt$.didbjs_weight) %*% z
  treated_idx <- which(dt$.didbjs_treated)
  untreated_idx <- which(dt$.didbjs_untreated)
  z0 <- z[untreated_idx, , drop = FALSE]
  s_z0z0 <- if (isTRUE(unit_weights)) {
    Matrix::crossprod(z0)
  } else {
    z0_weighted <- z_weighted[untreated_idx, , drop = FALSE]
    Matrix::crossprod(z0, z0_weighted)
  }
  if (!is.null(loadweights)) {
    if (!is.matrix(loadweights) || nrow(loadweights) != nrow(dt) || ncol(loadweights) != length(wtr_cols)) {
      stop_contract("Loaded weights do not match the current sample and terms.")
    }
    v_star <- loadweights * dt$.didbjs_weight
  } else if (length(controls) > 0) {
    v_star <- iterative_imputation_weights(
      dt = dt,
      wtr_cols = wtr_cols,
      controls = controls,
      fe_cols = fe_cols,
      tol = 1e-6,
      maxit = 1000
    )
    v_star <- v_star * dt$.didbjs_weight
  } else {
    wtr_mat <- Matrix::Matrix(as.matrix(dt[.didbjs_treated == TRUE, ..wtr_cols]), sparse = TRUE)
    z1 <- z[treated_idx, , drop = FALSE]
    z1_wtr <- Matrix::crossprod(z1, wtr_mat)
    solved <- solve_first_stage_system(s_z0z0, z1_wtr, z0)
    v_star <- -1 * z_weighted[, solved$keep, drop = FALSE] %*% solved$solution
    v_star[dt$.didbjs_treated == TRUE, ] <- as.matrix(dt[.didbjs_treated == TRUE, ..wtr_cols])
  }

  score_dt <- data.table::data.table(.didbjs_cluster = dt[[cluster]])
  residual_matrix <- matrix(NA_real_, nrow = nrow(dt), ncol = length(terms), dimnames = list(NULL, terms))
  for (idx in seq_along(terms)) {
    v_col <- didbjs_scratch_column("v_star", idx)
    tau_bar_col <- didbjs_scratch_column("tau_bar", idx)
    recentered_col <- didbjs_scratch_column("recentered", idx)
    score_col <- terms[[idx]]
    temp_cols <- c(v_col, tau_bar_col, recentered_col)

    dt[, (v_col) := as.numeric(v_star[, idx])]
    smart <- smart_treatment_components(
      dt = dt,
      v_col = v_col,
      cluster = cluster,
      avgeffectsby = avgeffectsby
    )
    if (isTRUE(leaveout) && smart$failure_count > 0) {
      stop_contract(
        "Cannot compute leave-out standard errors because of ",
        smart$failure_count,
        " observations for coefficient \"",
        score_col,
        "\"\nThis most likely happened because there are cohorts with only one unit or cluster (and the default value for avgeffectsby  is used).\nConsider using the avgeffectsby option with broader observation groups. Do not address this problem by using non-leave-out standard errors, as they may be downward biased for the same reason."
      )
    }
    dt[, (tau_bar_col) := smart$average]
    dt[, (recentered_col) := .didbjs_tau]
    dt[.didbjs_treated == TRUE, (recentered_col) := .didbjs_tau - get(tau_bar_col)]
    if (isTRUE(leaveout)) {
      scale_col <- didbjs_scratch_column("leaveout_scale", idx)
      temp_cols <- c(temp_cols, scale_col)
      dt[, (scale_col) := smart$leaveout_scale]
      dt[.didbjs_treated == TRUE, (recentered_col) := get(recentered_col) * get(scale_col)]
    }
    residual_matrix[, score_col] <- dt[[recentered_col]]
    score_dt[, (score_col) := dt[[v_col]] * dt[[recentered_col]]]
    existing_temp_cols <- intersect(temp_cols, names(dt))
    if (length(existing_temp_cols) > 0) {
      dt[, (existing_temp_cols) := NULL]
    }
  }

  score_terms <- terms
  if (length(controls) > 0) {
    control_scores <- control_cluster_scores(
      dt = dt,
      z = z,
      z0 = z0,
      s_z0z0 = s_z0z0,
      first_stage = first_stage,
      cluster = cluster,
      controls = controls,
      fe_cols = fe_cols
    )
    for (control in controls) {
      score_dt[, (control) := control_scores[[control]]]
    }
    score_terms <- c(score_terms, controls)
  }

  row_score_terms <- score_terms
  cluster_scores <- score_dt[, lapply(.SD, sum), by = .didbjs_cluster, .SDcols = row_score_terms]
  if (!is.null(extra_cluster_scores) && length(extra_terms) > 0) {
    cluster_scores <- merge(
      cluster_scores,
      extra_cluster_scores,
      by = ".didbjs_cluster",
      all = TRUE,
      sort = FALSE
    )
    for (term in extra_terms) {
      data.table::set(cluster_scores, which(is.na(cluster_scores[[term]])), term, 0)
    }
    for (term in row_score_terms) {
      data.table::set(cluster_scores, which(is.na(cluster_scores[[term]])), term, 0)
    }
    score_terms <- c(terms, extra_terms, controls)
  }
  scores <- as.matrix(cluster_scores[, ..score_terms])
  covariance <- crossprod(scores)
  dimnames(covariance) <- list(score_terms, score_terms)
  list(
    covariance = covariance,
    imputation_weights = as.matrix(v_star),
    residuals = residual_matrix
  )
}

didbjs_scratch_column <- function(stem, idx) {
  paste0(".didbjs_", stem, "_", idx)
}

smart_treatment_components <- function(dt, v_col, cluster, avgeffectsby) {
  average <- numeric(nrow(dt))
  leaveout_scale <- rep(1, nrow(dt))
  treated <- dt$.didbjs_treated == TRUE
  if (!any(treated)) {
    return(list(average = average, leaveout_scale = leaveout_scale, failure_count = 0L))
  }
  score_dt <- data.table::data.table(
    .didbjs_row = seq_len(nrow(dt)),
    .didbjs_cluster = dt[[cluster]],
    .didbjs_v = dt[[v_col]],
    .didbjs_tau = dt$.didbjs_tau
  )
  for (group_col in avgeffectsby) {
    score_dt[[group_col]] <- dt[[group_col]]
  }
  score_dt <- score_dt[which(treated)]
  cluster_group <- c(".didbjs_cluster", avgeffectsby)
  score_dt[
    ,
    .didbjs_clusterweight := sum(.didbjs_v, na.rm = TRUE),
    by = cluster_group
  ]
  score_dt[
    ,
    .didbjs_smartdenom := sum(.didbjs_clusterweight * .didbjs_v, na.rm = TRUE),
    by = avgeffectsby
  ]
  score_dt[, .didbjs_smartweight := 0]
  valid <- is.finite(score_dt$.didbjs_smartdenom) & abs(score_dt$.didbjs_smartdenom) > .Machine$double.eps
  score_dt[
    valid == TRUE,
    .didbjs_smartweight := .didbjs_clusterweight * .didbjs_v / .didbjs_smartdenom
  ]
  score_dt[
    ,
    .didbjs_tau_smartavg := sum(.didbjs_tau * .didbjs_smartweight, na.rm = TRUE),
    by = avgeffectsby
  ]
  failure <- score_dt$.didbjs_smartdenom > 0 &
    (score_dt$.didbjs_clusterweight^2) / score_dt$.didbjs_smartdenom > 0.99999
  scale_valid <- score_dt$.didbjs_smartdenom > 0 &
    abs(score_dt$.didbjs_smartdenom - score_dt$.didbjs_clusterweight^2) > .Machine$double.eps
  score_dt[, .didbjs_leaveout_scale := 1]
  score_dt[
    scale_valid == TRUE,
    .didbjs_leaveout_scale := .didbjs_smartdenom / (.didbjs_smartdenom - .didbjs_clusterweight^2)
  ]
  average[score_dt$.didbjs_row] <- score_dt$.didbjs_tau_smartavg
  leaveout_scale[score_dt$.didbjs_row] <- score_dt$.didbjs_leaveout_scale
  list(
    average = average,
    leaveout_scale = leaveout_scale,
    failure_count = sum(failure, na.rm = TRUE)
  )
}

iterative_imputation_weights <- function(dt,
                                         wtr_cols,
                                         controls,
                                         fe_cols,
                                         tol = 1e-6,
                                         maxit = 1000) {
  weights <- as.matrix(dt[, ..wtr_cols]) / dt$.didbjs_weight
  storage.mode(weights) <- "double"
  active <- seq_along(wtr_cols)
  untreated <- dt$.didbjs_untreated == TRUE
  wei <- dt$.didbjs_weight

  control_dm <- list()
  control_denom <- numeric(length(controls))
  names(control_denom) <- controls
  for (control in controls) {
    centered <- dt[[control]] - stats::weighted.mean(dt[[control]][untreated], wei[untreated])
    denom <- sum(wei[untreated] * centered[untreated]^2)
    control_dm[[control]] <- centered
    control_denom[[control]] <- denom
  }

  fe_infos <- list()
  for (fe_col in fe_cols) {
    group <- factor(dt[[fe_col]], exclude = NULL)
    group_index <- as.integer(group)
    denom <- as.numeric(rowsum(wei * as.numeric(untreated), group_index, reorder = TRUE))
    fe_infos[[fe_col]] <- list(
      group_index = group_index,
      denom = denom
    )
  }

  iteration <- 0L
  while (iteration < maxit && length(active) > 0) {
    old_weights <- weights[untreated, active, drop = FALSE]

    for (control in controls) {
      denom <- control_denom[[control]]
      if (!is.finite(denom) || abs(denom) <= .Machine$double.eps) {
        next
      }
      centered <- control_dm[[control]]
      sumw <- colSums(weights[, active, drop = FALSE] * (wei * centered))
      weights[untreated, active] <- weights[untreated, active, drop = FALSE] -
        tcrossprod(centered[untreated] / denom, sumw)
    }

    for (fe_col in fe_cols) {
      fe_info <- fe_infos[[fe_col]]
      if (is.null(fe_info) || length(fe_info$denom) == 0) {
        next
      }
      weighted_active <- weights[, active, drop = FALSE] * wei
      sum_by_group <- rowsum(weighted_active, fe_info$group_index, reorder = TRUE)
      untreated_group <- fe_info$group_index[untreated]
      adjustment <- sum_by_group[untreated_group, , drop = FALSE] / fe_info$denom[untreated_group]
      adjustment[!is.finite(adjustment)] <- 0
      weights[untreated, active] <- weights[untreated, active, drop = FALSE] - adjustment
    }

    diffs <- colSums(abs(old_weights - weights[untreated, active, drop = FALSE]))
    active <- active[diffs > tol]
    iteration <- iteration + 1L
  }

  if (length(active) > 0) {
    stop_contract("Convergence of standard errors was not achieved for controls after ", maxit, " iterations.")
  }
  weights
}

control_cluster_scores <- function(dt, z, z0, s_z0z0, first_stage, cluster, controls, fe_cols) {
  control_cols <- match(controls, colnames(z))
  if (anyNA(control_cols)) {
    stop_contract(
      "Could not run imputation for some observations because some controls are collinear in the D==0 subsample but not in the full sample"
    )
  }
  rhs <- Matrix::Diagonal(n = ncol(z))[, control_cols, drop = FALSE]
  solved <- solve_first_stage_system(s_z0z0, rhs, z0)
  influence <- as.matrix(z[, solved$keep, drop = FALSE] %*% solved$solution)
  residuals <- numeric(nrow(dt))
  residuals[dt$.didbjs_untreated == TRUE] <- as.numeric(stats::residuals(first_stage))
  scores <- influence * dt$.didbjs_weight * residuals
  scores[dt$.didbjs_untreated != TRUE, ] <- 0
  scores <- scores * sqrt(control_dof_adjustment(dt, z0, z, cluster, controls, fe_cols))
  colnames(scores) <- controls
  as.data.frame(scores, stringsAsFactors = FALSE)
}

solve_first_stage_system <- function(s_z0z0, rhs, z0) {
  full <- tryCatch(
    suppressWarnings(Matrix::solve(s_z0z0, rhs)),
    error = function(err) NULL
  )
  if (!is.null(full)) {
    return(list(solution = full, keep = seq_len(ncol(s_z0z0))))
  }
  keep <- independent_design_columns(z0)
  if (length(keep) == 0) {
    stop_contract("First-stage design matrix is rank deficient.")
  }
  reduced <- tryCatch(
    suppressWarnings(Matrix::solve(s_z0z0[keep, keep, drop = FALSE], rhs[keep, , drop = FALSE])),
    error = function(err) {
      stop_contract("First-stage design matrix is rank deficient.")
    }
  )
  list(solution = reduced, keep = keep)
}

independent_design_columns <- function(z0) {
  if (inherits(z0, "sparseMatrix")) {
    if (prod(dim(z0)) <= .didbjs_sparse_dense_fallback_max_entries) {
      return(independent_dense_design_columns(z0))
    }
    keep <- independent_sparse_design_columns(z0)
    if (!is.null(keep)) {
      return(keep)
    }
    stop_contract("First-stage design matrix is rank deficient, and sparse fallback rank resolution would require densifying more than 5e6 entries.")
  }
  independent_dense_design_columns(z0)
}

independent_dense_design_columns <- function(z0) {
  qr_obj <- qr(as.matrix(z0), tol = 1e-10)
  if (qr_obj$rank == 0) {
    return(integer())
  }
  sort(qr_obj$pivot[seq_len(qr_obj$rank)])
}

independent_sparse_design_columns <- function(z0) {
  # Matrix::qr warns for structurally rank-deficient sparse matrices, which is
  # exactly the fallback case here. Rank is determined from the R diagonal below.
  qr_obj <- tryCatch(
    suppressWarnings(Matrix::qr(z0)),
    error = function(err) NULL
  )
  if (is.null(qr_obj) || !inherits(qr_obj, "sparseQR")) {
    return(NULL)
  }
  r_diag <- abs(Matrix::diag(qr_obj@R))
  if (length(r_diag) == 0) {
    return(integer())
  }
  tol <- 1e-10 * max(1, max(r_diag, na.rm = TRUE))
  keep_pos <- which(is.finite(r_diag) & r_diag > tol)
  if (length(keep_pos) == 0) {
    return(integer())
  }
  sort(qr_obj@q[keep_pos] + 1L)
}

control_dof_adjustment <- function(dt, z0, z, cluster, controls, fe_cols) {
  untreated <- dt$.didbjs_untreated == TRUE
  n_control <- sum(untreated)
  n_clusters <- data.table::uniqueN(dt[[cluster]][untreated])
  if (n_clusters < 2) {
    stop_contract("Clustered covariance requires at least two clusters.")
  }
  df_m <- length(controls)
  df_a <- absorbed_df_for_cluster(dt[untreated], z0, z, cluster, controls, fe_cols)
  denom <- n_control - df_m - df_a
  if (!is.finite(denom) || denom <= 0) {
    stop_contract("Control covariance degrees of freedom are not positive.")
  }
  (n_control - 1) / denom * n_clusters / (n_clusters - 1)
}

absorbed_df_for_cluster <- function(untreated_dt, z0, z, cluster, controls, fe_cols) {
  if (length(fe_cols) == 0) {
    return(0)
  }
  z_names <- colnames(z)
  fe_cols_in_z <- setdiff(seq_along(z_names), match(controls, z_names, nomatch = 0))
  if (length(fe_cols_in_z) == 0) {
    return(0)
  }
  nested_cols <- integer()
  for (fe_col in fe_cols) {
    if (!fe_col %in% names(untreated_dt)) {
      next
    }
    cluster_counts <- untreated_dt[, .(.didbjs_cluster_count = data.table::uniqueN(get(cluster))), by = fe_col]
    is_nested <- all(cluster_counts$.didbjs_cluster_count == 1)
    if (is_nested) {
      nested_cols <- c(nested_cols, grep(paste0("^", fe_col, "::"), z_names))
    }
  }
  keep_cols <- setdiff(fe_cols_in_z, nested_cols)
  if (length(keep_cols) == 0) {
    return(0)
  }
  Matrix::rankMatrix(z0[, keep_cols, drop = FALSE], method = "qr")[1]
}

tidy <- function(x, ...) {
  UseMethod("tidy")
}

tidy.didbjs <- function(x, ...) {
  as.data.frame(upgrade_didbjs_object(x))
}

glance <- function(x, ...) {
  UseMethod("glance")
}

glance.didbjs <- function(x, ...) {
  x <- upgrade_didbjs_object(x)
  estimates <- x$estimates
  pretrends <- x$diagnostics$pretrends
  data.frame(
    n_terms = nrow(estimates),
    n_obs = sum(x$sample_mask$sample),
    n_control = unique(estimates$n_control)[[1]],
    n_treated = sum(estimates$n_treated, na.rm = TRUE),
    n_controls = nrow(x$controls),
    has_artifacts = length(x$artifacts) > 0,
    has_pretrends = is.numeric(pretrends) && length(pretrends) == 1L && !is.na(pretrends) && pretrends > 0,
    object_version = attr(x, "didbjs_object_version") %||% "",
    stringsAsFactors = FALSE
  )
}

as.data.frame.didbjs <- function(x, row.names = NULL, optional = FALSE, ...) {
  x <- upgrade_didbjs_object(x)
  out <- x$estimates
  if (!is.null(row.names)) {
    row.names(out) <- row.names
  }
  out
}

coef.didbjs <- function(object, ...) {
  object <- upgrade_didbjs_object(object)
  stats::setNames(object$estimates$estimate, object$estimates$term)
}

vcov.didbjs <- function(object, ...) {
  object <- upgrade_didbjs_object(object)
  object$covariance
}

summary.didbjs <- function(object, ...) {
  object <- upgrade_didbjs_object(object)
  structure(
    list(
      estimates = object$estimates,
      controls = object$controls,
      diagnostics = object$diagnostics
    ),
    class = "summary.didbjs"
  )
}

print.summary.didbjs <- function(x, ...) {
  cat("<summary.didbjs>\n")
  print(x$estimates, row.names = FALSE)
  if (nrow(x$controls) > 0) {
    cat("\nControls:\n")
    print(x$controls, row.names = FALSE)
  }
  invisible(x)
}

upgrade_didbjs_object <- function(x) {
  if (!is.list(x)) {
    stop_contract("A didbjs object must be a list.")
  }
  required <- c("estimates", "covariance", "sample_mask")
  missing_required <- setdiff(required, names(x))
  if (length(missing_required) > 0) {
    stop_contract("Cannot upgrade didbjs object; missing fields: ", paste(missing_required, collapse = ", "))
  }
  if (is.null(x$controls)) {
    x$controls <- data.frame(
      term = character(),
      estimate = numeric(),
      std.error = numeric(),
      conf.low = numeric(),
      conf.high = numeric(),
      stringsAsFactors = FALSE
    )
  }
  if (is.null(x$artifacts)) {
    x$artifacts <- list()
  }
  if (is.null(x$diagnostics)) {
    x$diagnostics <- list()
  }
  if (is.null(x$call)) {
    x$call <- NULL
  }
  class(x) <- "didbjs"
  attr(x, "didbjs_object_version") <- "didbjs.result.v1"
  x
}

print.didbjs <- function(x, ...) {
  x <- upgrade_didbjs_object(x)
  cat("<didbjs>\n")
  print(x$estimates, row.names = FALSE)
  invisible(x)
}

stop_contract <- function(...) {
  stop(structure(list(message = paste0(...)), class = c("didbjs_contract_error", "error", "condition")))
}

stop_unsupported <- function(...) {
  stop(structure(list(message = paste0(...)), class = c("didbjs_unsupported_error", "error", "condition")))
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
