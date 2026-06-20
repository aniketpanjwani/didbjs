\name{event_plot}
\alias{event_plot}
\title{Event-study plot with extractable plot data}
\usage{
event_plot(results_obj = NULL, pretrends = NULL, pretrends_std = NULL,
  effects = NULL, effects_std = NULL, models = NULL,
  stub_lag = "tau#", stub_lead = "pre#",
  trimlag = NULL, trimlead = NULL, shift = 0, perturb = NULL,
  model_names = NULL, plot_type = "rcap", significance_level = 0.05,
  pretrends_marker = 16, effects_marker = 15,
  pretrends_color = "blue", effects_color = "red",
  effects_linestyle = "solid", pretrends_linestyle = "solid",
  figsize = c(12, 8), title = "",
  xlabel = "Time Relative to Treatment", ylabel = "Coefficient",
  show_grid = TRUE, show_legend = TRUE, show_zero_line = TRUE,
  zero_line_position = 0, zero_line_style = "dashed",
  zero_line_color = "grey", zero_line_width = 1,
  zero_line_alpha = 0.5, show_event_line = TRUE,
  event_line_position = 0, event_line_style = "dashed",
  event_line_color = "black", event_line_width = 1,
  event_line_alpha = 0.5, pretrends_marker_size = 2.8,
  effects_marker_size = 2.8, pretrends_line_width = 0.7,
  effects_line_width = 0.7, effects_alpha = 0.7,
  pretrends_alpha = 0.7, pretrends_rarea_color = "blue",
  effects_rarea_color = "red", pretrends_rarea_alpha = 0.3,
  effects_rarea_alpha = 0.3, pretrends_label = "Pre-trends",
  effects_label = "Effects", font_size = 12, legend_loc = "best",
  save_path = NULL, dpi = 300, together = FALSE, noplot = FALSE,
  overwrite = TRUE, ...)
}
\arguments{
\item{results_obj}{Optional \code{DIDImputationOutput} or \code{didbjs} object.}
\item{pretrends, effects}{Named numeric vectors or lists such as \code{pre1},
\code{pre2}, \code{tau0}, and \code{tau1}.}
\item{pretrends_std, effects_std}{Optional named standard-error vectors or
lists with names matching the corresponding estimates.}
\item{models}{Optional list of Stata-like model specifications. Each model is a
list with named \code{estimates} and either named \code{std_errors} or a named
\code{vcov} matrix.}
\item{stub_lag, stub_lead}{Stata-like lag/effect and lead/pretrend stubs using
one \code{#} placeholder. Values may be length one or one per model.}
\item{trimlag, trimlead}{Optional maximum lag and lead event times retained
after stub extraction.}
\item{shift}{Per-model event-time shift used in Stata positions:
\code{position = event_time + perturb - shift}.}
\item{perturb}{Per-model horizontal perturbation. Defaults to Stata-like overlay
offsets when multiple models are supplied.}
\item{model_names}{Optional labels for model overlays.}
\item{plot_type}{Either \code{"rcap"} for capped intervals or \code{"rarea"} for
confidence bands.}
\item{significance_level}{Two-sided confidence level alpha.}
\item{pretrends_marker, effects_marker}{Point shapes for pretrend and effect
series.}
\item{pretrends_color, effects_color}{Line and point colors for pretrend and
effect series.}
\item{effects_linestyle, pretrends_linestyle}{Line types for effect and
pretrend series.}
\item{figsize}{Width and height in inches used when saving.}
\item{title, xlabel, ylabel}{Plot title and axis labels.}
\item{show_grid, show_legend}{Whether to show the panel grid and legend.}
\item{show_zero_line, zero_line_position, zero_line_style, zero_line_color,
zero_line_width, zero_line_alpha}{Horizontal reference-line controls.}
\item{show_event_line, event_line_position, event_line_style, event_line_color,
event_line_width, event_line_alpha}{Vertical event-line controls.}
\item{pretrends_marker_size, effects_marker_size}{Point sizes.}
\item{pretrends_line_width, effects_line_width}{Series line widths.}
\item{effects_alpha, pretrends_alpha}{Series alpha values.}
\item{pretrends_rarea_color, effects_rarea_color, pretrends_rarea_alpha,
effects_rarea_alpha}{Confidence-band fill controls for \code{plot_type =
"rarea"}.}
\item{pretrends_label, effects_label}{Series labels used in plot data and the
legend.}
\item{font_size}{Base font size for the \code{ggplot2} theme.}
\item{legend_loc}{Legend position. \code{"best"} uses the default ggplot
position.}
\item{save_path}{Optional file path passed to \code{ggplot2::ggsave}.}
\item{dpi}{Saved plot resolution.}
\item{together}{Whether pretrend and effect rows are drawn as one effect series.}
\item{noplot}{Whether to skip rendering and return plot data only.}
\item{overwrite}{Whether an existing \code{save_path} may be overwritten.}
\item{...}{Unsupported arguments currently error.}
}
\description{
Builds Python-compatible event-study plot data and an R-native \code{ggplot}
rendering. The returned object always contains \code{plot_data} with event-time
coordinates, estimates, standard errors, critical values, confidence interval
coordinates, and series labels. This implements the D013 semantic parity
contract; literal Stata graph-option syntax is intentionally outside v1.
Malformed manual inputs fail closed with structured errors for mismatched term
names, partial standard-error vectors, missing coefficients, duplicate terms,
non-finite numeric values, alpha boundaries, and unsupported keyword arguments.
Stata-like multi-model inputs support up to eight overlaid models with
length-one or per-model stubs, trim bounds, shifts, perturbations, and model
labels; over-eight overlays and stub collisions fail closed.
When \code{save_path} is supplied, parent directories and overwrite policy are
validated before rendering; invalid paths fail with structured errors and
successful saves leave graphics devices and the global \code{ggplot2} theme
unchanged.
}
\value{
A \code{didbjs_event_plot} object with \code{plot}, \code{plot_data},
\code{plot_type}, \code{together}, and \code{save_path}.
}
