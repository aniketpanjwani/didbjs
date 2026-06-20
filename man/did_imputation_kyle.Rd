\name{did_imputation_kyle}
\alias{did_imputation_kyle}
\title{Kyle-compatible did_imputation wrapper}
\usage{
did_imputation_kyle(data, yname, gname, tname, idname,
  first_stage = NULL, wname = NULL, wtr = NULL, horizon = NULL,
  pretrends = NULL, cluster_var = NULL, subset = NULL)
}
\arguments{
\item{data}{A data frame.}
\item{yname}{Outcome column name, using Kyle's argument name. Multiple outcomes may use Kyle/fixest syntax such as \code{"c(Y, Y2)"}.}
\item{gname}{Treatment timing column name.}
\item{tname}{Time column name.}
\item{idname}{Unit id column name.}
\item{first_stage}{Optional Kyle-style first-stage RHS as a one-sided formula
or single string, for example \code{~ x1 + x2 | unit + t} or
\code{"x1 + x2 | unit + t"}. RC-v1 supports plain column names on the
control and fixed-effect sides.}
\item{wname}{Analytic weight column name. Missing weights are excluded by the
underlying \code{didbjs} estimator; non-missing analytic weights must be positive
and finite.}
\item{wtr}{Optional Kyle-style custom treatment-weight column name or vector of
column names.}
\item{horizon}{Optional non-negative integer horizons. Explicit empty vectors and absent horizons fail closed; \code{TRUE} all-horizon discovery remains unsupported in RC-v1.}
\item{pretrends}{Optional Kyle-style negative event-time vector such as \code{c(-2, -1)} or \code{TRUE}. Returned rows use Kyle's negative event-time term names.}
\item{cluster_var}{Cluster column name. Defaults to \code{idname}.}
\item{subset}{Optional R-side subset convenience. The pinned Kyle reference has no \code{subset} argument, so reference fixtures prefilter the same rows before calling Kyle.}
}
\description{
Compatibility wrapper for Kyle Butts's \code{didimputation} call shape. The
current implementation covers the static ATT path used by F001 and the
explicit dynamic-horizon path used by F002, plus the analytic-weight static
path used by F007, Kyle-style pretrend rows used by F015, and the F040 exact
Kyle public-call fixture covering positional and named calls, first-stage
formulas, \code{wname}, \code{wtr}, horizons, pretrends, clustering,
multi-outcome output, class, and column order. It returns
Kyle-style columns: \code{term}, \code{estimate}, \code{std.error},
\code{conf.low}, and \code{conf.high}. For Kyle-style multi-outcome calls, it
adds the leading \code{lhs} column and returns one row per outcome/term.
}
\details{
The pinned Kyle reference has an upstream data.table NSE collision when a data
column is literally named \code{i}. This wrapper accepts \code{idname = "i"} and
routes through \code{didbjs}'s internal estimator, matching the Kyle alias-probe
fixture that removes the name collision while preserving the data.
For F007, Kyle and Stata agree on the analytic-weight point estimate but not on
the clustered standard error. D018 records that Stata governs the wrapper's
numeric SE while the wrapper preserves Kyle's output shape.
All confidence intervals returned by this wrapper use Kyle's
\code{estimate +/- 1.96 * std.error} convention, including ATT,
dynamic-horizon, and pretrend rows. The wrapper does not expose
\code{significance_level}; use \code{did_imputation()} directly when confidence
intervals must vary with alpha.
For F015 unweighted pretrend rows, the wrapper preserves Kyle's negative
event-time row labels, pretrend standard errors, and Kyle's
\code{estimate +/- 1.96 * std.error} confidence-interval convention.
For F021 multi-outcome calls, the wrapper matches Kyle's \code{lhs} output and
verifies each outcome against its single-outcome probe.
For F025 missingness/subset calls, the wrapper supports a static Kyle-compatible
fallback that matches the pinned Kyle public estimate and \code{NA} standard
error shape after applying the same subset prefilter.
For F026 treatment-timing calls, the wrapper maps \code{gname == 0} to
never-treated before routing to the estimator, matching Kyle's zero-as-never
compatibility convention.
For F030 horizon-boundary calls, the wrapper preserves Kyle-style sorted dynamic
term order for valid horizons but rejects absent and empty explicit horizons
instead of reproducing Kyle's \code{NA} or vacuous all-horizon behavior.
}
\value{
A \code{data.table} with Kyle-style static or dynamic ATT output, including an
\code{lhs} column for multi-outcome calls.
}
