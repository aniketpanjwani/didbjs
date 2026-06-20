\name{did_imputation_python}
\alias{did_imputation_python}
\title{Python-compatible did_imputation wrapper}
\usage{
did_imputation_python(df, y, i, t, Ei, controls = character(),
  fe = character(), timecontrols = character(), aw = NULL,
  unitcontrols = character(), wtr = character(), sum = FALSE,
  horizons = character(), allhorizons = FALSE, hbalance = FALSE,
  hetby = character(), project = character(), minn = 30,
  saveweights = FALSE, shift = 0, pretrends = 0, cluster = "",
  avgeffectsby = character(), leaveoneout = FALSE, nose = FALSE,
  delta = NULL, subset = NULL)
}
\arguments{
\item{df}{A data frame.}
\item{y}{Outcome column name.}
\item{i}{Unit id column name.}
\item{t}{Time column name.}
\item{Ei}{Treatment timing column name.}
\item{controls}{Continuous control column names for the static ATT path.}
\item{fe}{Python-compatible fixed-effect list. Empty means default unit and time FE; \code{NULL} means constant-only.}
\item{timecontrols, unitcontrols, hetby, project}{Python placeholder arguments; unsupported in the pinned Python reference.}
\item{aw}{Analytic weight column name. Missing weights are excluded with
\code{sample_mask} diagnostics in the underlying \code{didbjs} result; non-missing
analytic weights must be positive and finite.}
\item{horizons}{Optional non-negative integer horizons. Empty vectors preserve the omitted Python default and return static ATT; duplicate, negative, and absent explicit horizons fail closed before permissive Python edge behavior is reached.}
\item{allhorizons}{Whether to discover all non-negative treated horizons from the sample. Cannot be combined with \code{horizons}.}
\item{hbalance}{Whether horizon estimands use only units observed for every requested horizon.}
\item{wtr}{Optional custom positive treatment-weight column names. The wrapper returns Python-compatible raw \code{wtr} estimate names.}
\item{pretrends}{Non-negative integer count of pretrend coefficients. The wrapper returns Python-compatible \code{pretrends_estimates} and \code{pretrends_std_errors} fields.}
\item{saveweights}{Whether to populate the Python-compatible \code{weights} field for the current saveweights path.}
\item{sum, shift, avgeffectsby, leaveoneout, nose, delta}{Reserved Python-compatible options not yet implemented in the current path.}
\item{minn}{Minimum-support setting.}
\item{cluster}{Cluster column name. Empty string defaults to \code{i}, matching Python.}
\item{subset}{Optional R-side subset convenience. The pinned Python reference has no \code{subset} argument, so reference fixtures prefilter the same rows before calling Python.}
}
\description{
Compatibility wrapper for the pinned Python \code{did_imputation} API. The
current implementation covers the F001 static ATT path, the F002 explicit
dynamic-horizon path, the F003 all-horizon discovery path, the F004
horizon-balance path, the F005 custom positive weight path, the F007
analytic-weight path, the F008 alternative-FE path, and the F009
continuous-control path, plus later core fixture paths through F015 pretrends
and the F018 \code{saveweights} output field. F025 covers the wrapper's
\code{subset} prefilter equivalence to the same prefiltered Python reference,
returning an object with
Python-compatible field names and nullability.
F030 covers horizon-boundary behavior: unsorted and sparse vectors preserve
Python-compatible term order, while duplicate, negative, absent, and ambiguous
\code{horizons}/\code{allhorizons} inputs are guarded by structured errors
except for the empty-vector static default.
F031 covers rank-deficient FE arguments. Nested, duplicate, singleton, and
disconnected FE cases preserve Python-compatible object shape on successful
estimates, while absorbed controls and untreated-sample rank failures raise
structured \code{didbjs_contract_error} conditions under D024.
F032 covers identification failures. The wrapper preserves dynamic autosample
behavior from F013, but static ATT calls that autosample away or trim the
requested estimand fail closed under D025.
}
\details{
For F001-F005, F007, and F009, the pinned Python reference returns point estimates
outside the frozen TOL001 tolerance because of its iterative fixed-effect
recovery. D017 records this as an approved numerical divergence: \code{didbjs}
uses the Stata/algebraic/Kyle core estimates while preserving
Python-compatible argument and object-field shape.
}
\value{
A list with class \code{DIDImputationOutput} and fields matching the pinned
Python object: \code{pretrends_estimates}, \code{pretrends_std_errors},
\code{estimates}, \code{std_errors}, \code{controls_estimates},
\code{controls_std_errors}, \code{n_obs}, \code{weights}, and \code{V}.
}
