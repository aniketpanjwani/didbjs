\name{did_imputation}
\alias{did_imputation}
\title{BJS imputation estimator}
\usage{
did_imputation(data, y, i, t, Ei, controls = NULL, fe = NULL,
  aw = NULL, cluster = NULL, subset = NULL, avgeffectsby = NULL,
  leaveout = FALSE, saveweights = FALSE, loadweights = NULL,
  saveestimates = FALSE, saveresid = FALSE, hetby = NULL, project = NULL,
  minn = 30, autosample = FALSE, wtr = NULL, sum = FALSE, horizons = NULL,
  allhorizons = FALSE, hbalance = FALSE, pretrends = 0, shift = 0,
  delta = NULL, significance_level = 0.05, ...)
}
\arguments{
\item{data}{A data frame.}
\item{y}{Outcome column name.}
\item{i}{Unit id column name.}
\item{t}{Time column name.}
\item{Ei}{Treatment timing column name.}
\item{controls}{Optional continuous control column names included in the untreated first-stage model.}
\item{fe}{Fixed-effect column names. If omitted, unit and time fixed effects are used. Use \code{NULL} explicitly for constant-only.}
\item{aw}{Analytic weight column name. Missing weights are excluded with
\code{sample_mask} diagnostics; non-missing analytic weights must be positive
and finite. Other Stata-style weight classes such as importance and frequency
weights are outside v1.}
\item{cluster}{Cluster column name. Defaults to \code{i}.}
\item{subset}{Optional sample filter. Supply \code{NULL}, a logical vector, a finite 0/1 vector, or a single column name. Rows outside the subset and rows missing required outcome, id, time, controls, fixed effects, cluster, weights, or custom-weight columns are represented in \code{sample_mask}; missing \code{Ei} remains the never-treated encoding.}
\item{avgeffectsby}{Optional grouping columns for Stata-style treatment-effect recentering in the covariance. The default is cohort-year grouping, equivalent to \code{c(Ei, t)}.}
\item{leaveout}{Whether to apply Stata-style leave-out standard error scaling. Singleton/cohort cases that Stata rejects also fail with matching user guidance; use broader \code{avgeffectsby} groups when appropriate.}
\item{saveweights}{Whether to return Stata-style imputation weights in \code{artifacts$weights}. The artifact uses stable \code{row_id}, \code{term}, and \code{weight} columns and includes schema/spec metadata. If the input contains a unique \code{row_id} column, that column is the artifact key; otherwise \code{didbjs} generates positional row ids from the original input order.}
\item{loadweights}{Optional \code{didbjs_weights} artifact, a prior \code{didbjs} result containing one, or a compatible \code{row_id}/\code{term}/\code{weight} data frame used for covariance on a second outcome. Bare data-frame weights are treated as a warned manual override because they cannot carry spec-hash metadata; they must still cover every required row/term pair with finite weights. The manual override condition has class \code{didbjs_manual_loadweights_warning}; when \code{getOption("warn") >= 2}, valid manual loads still signal that class without converting the load into an error.}
\item{saveestimates}{Whether to return observation-level imputed treatment effects in \code{artifacts$estimates}. Untreated rows are explicit with missing estimates.}
\item{saveresid}{Whether to return Stata-style residual components used for standard error construction in \code{artifacts$residuals}.}
\item{hetby}{Optional subgroup column for Stata-style heterogeneous estimands. Values on treated observations must be non-negative integers; estimates are named \code{tau_<level>} for the static path.}
\item{project}{Optional numeric column names for Stata-style projection of treatment effects. The static path returns \code{tau_cons} and one slope term per projection variable, dropping collinear projection coefficients.}
\item{minn}{Minimum effective treated-observation support. The default \code{30} suppresses low-support terms as explicit rows with missing native estimate/SE values and suppression diagnostics. Use \code{0} to report all terms.}
\item{autosample}{Whether to drop non-imputable treated rows that enter an estimand. The R-native default is fail-closed; \code{TRUE} records cannot-impute row ids, Stata-style trim/drop diagnostics, and renormalized weights.}
\item{wtr}{Optional custom treatment-weight column names. Positive weights with
\code{sum = FALSE} are normalized into weighted averages and are invariant to
constant scaling; missing values are excluded through \code{sample_mask};
nonzero values on untreated rows do not affect the estimand; all-zero custom
weights return Stata-style omitted zero output. With \code{sum = TRUE}, raw
analytic-weighted custom weights define weighted sums and may be negative.
Multiple columns return joint estimates using Stata term names
\code{tau_<wtr>}.}
\item{sum}{Whether custom \code{wtr} columns define weighted sums rather than normalized weighted averages. Currently implemented for custom \code{wtr} without horizons.}
\item{horizons}{Optional non-negative integer horizons. When supplied, returns one row per \code{tauh}. Unsorted and sparse vectors are allowed; duplicate, negative, absent, and empty explicit vectors fail closed with \code{didbjs_contract_error}.}
\item{allhorizons}{Whether to discover all non-negative treated horizons from the sample. Cannot be combined with \code{horizons}.}
\item{hbalance}{Whether horizon estimands use only units observed for every requested horizon.}
\item{pretrends}{Non-negative integer count of pretrend coefficients to estimate as \code{pre1}, \code{pre2}, and so on. Pretrend rows use untreated observations only and are appended after post-treatment terms.}
\item{shift}{Integer anticipation shift added to \code{t - Ei} before event-time classification.}
\item{delta}{Positive integer time step used to scale \code{t - Ei + shift}. \code{NULL} and \code{0} use Stata's default unit step of 1 in the current no-\code{tsset} profile.}
\item{significance_level}{Two-sided confidence-interval alpha. The R-native estimator follows Stata's normal critical-value semantics with infinite CI degrees of freedom.}
\item{...}{Unsupported arguments currently error.}
}
\description{
Implements the initial static ATT, explicit dynamic-horizon, all-horizon
discovery, horizon-balanced dynamic effects, and custom positive treatment
weights plus custom weighted-sum differences for the repository's frozen
conformance profile. Analytic weights are covered for the implemented static,
dynamic, custom-weight, and difference paths, including scaling invariance,
missing-weight sample-mask exclusion, and strict positive-finite validation.
Custom treatment weights cover Stata-style scaling invariance, multiple weight
columns, missing-weight row exclusion, zero-weight omitted output, untreated-row
support invariance, and signed zero-sum \code{sum = TRUE} estimands.
Transformation-invariance fixtures cover row order, unit labels, time
translations, outcome scaling, constant outcome shifts under fixed effects, and
analytic-weight rescaling.
R-native object behavior is covered for multiple unit-id storage types,
data.frame, data.table, and tibble-like inputs, nonsyntactic column names, row
names, and input immutability.
Alternative FE sets are covered for constant-only, unit-only, time-only, and
arbitrary FE-list specifications.
Continuous controls are covered for the static ATT path, including reported
control coefficients and covariance entries.
Repeated cross sections are covered when the caller supplies explicit stable
group fixed effects, such as \code{fe = c("region", "t")}, and a matching group
cluster.
Triple-difference specifications are covered for composite unit ids and
Stata-style interacted fixed effects such as
\code{fe = c("ig", "county#t", "g#t")}.
Shift and delta event-time specifications are covered for dynamic horizons,
including Stata-compatible non-integer event-time validation.
Failed-imputation behavior is covered with the R-native fail-closed default and
explicit \code{autosample = TRUE} trimming/dropping diagnostics.
Low-support \code{minn} suppression is covered with explicit native rows,
missing native estimate/SE values, zeroed internal weights, and compatibility
zeroing for Python-style output.
Pretrend estimation is covered for untreated-only regressions with Stata-style
\code{pre1}, \code{pre2}, full covariance entries, and joint-test diagnostics.
Small-cohort inference is covered for the default cohort-year recentering and
custom \code{avgeffectsby} groupings.
Leave-out standard errors are covered for successful broader
\code{avgeffectsby} groups and Stata-compatible singleton/cohort failure
guidance.
Saved imputation weights and second-outcome \code{loadweights} reuse are covered
for the static and dynamic artifact paths with stable row identity, dense and
sparse row-ID keyed representations, serialization-safe metadata, modified-sample
reuse checks, and input non-mutation.
Saved observation-level treatment effects and Stata-style SE residual
components are covered for the static path.
Heterogeneity by subgroup and projection of static treatment effects are
covered through Stata-style \code{hetby} and \code{project} options, including
invalid-combination diagnostics. F020 records a controlled Stata 14.2 runtime
divergence for direct \code{hetby}; \code{didbjs} implements the pinned ado's
source-level subgroup semantics and verifies estimates against the equivalent
split-weight Stata oracle.
Missingness and subset semantics are covered for the static controls path:
the result exposes original-row sample, subset, and missing-required masks while
retaining missing treatment timing as the never-treated encoding.
Treatment-timing encodings are covered for \code{NA}, \code{Inf}, Stata-style
tagged missing values, after-sample dates, before-sample negative dates, and
within-unit inconsistency checks. The R-native API rejects zero treatment timing;
use the Kyle compatibility wrapper for zero-as-never-treated migration calls.
Irregular and unbalanced panels are covered for gaps in time, nonconsecutive
periods, and unequal panel lengths. Duplicate \code{i}/\code{t} rows after
\code{subset} and missing-value filters fail closed with a
\code{didbjs_contract_error} that names the duplicate row ids.
Horizon input boundaries are covered for unsorted and sparse explicit requests,
duplicate and negative validation, absent horizon support, empty explicit
vectors, and \code{horizons}/\code{allhorizons} exclusivity. D023 records the
fail-closed divergences from permissive upstream absent/empty horizon behavior.
Fixed-effect rank pathologies are covered for nested FE, duplicate FE,
singleton FE levels, and disconnected FE components. D024 records the
fail-closed absorbed-control policy: controls collinear in the untreated
first-stage sample raise \code{didbjs_contract_error} instead of reporting
Stata's omitted zero control row.
Identification failures are covered for no untreated observations, no treated
observations, all units in one cohort, all post periods treated, and requested
horizons with zero treated support. D025 records the fail-closed divergence from
upstream omitted-zero outputs for unidentified designs.
Clustered inference boundaries are covered for one cluster, two clusters,
singleton clusters, missing cluster IDs, nested clusters, and alternative
cluster-label ordering. D009 records Stata-governed finite-sample covariance
semantics and structured one-cluster errors.
Confidence intervals use Stata's \code{alpha} semantics through
\code{significance_level}: alpha must be strictly between 0 and 1, CI
coordinates for estimates, controls, and native pretrend rows use
\code{qnorm(1 - alpha / 2)}, and the R-native diagnostics record the normal
critical value and infinite CI degrees of freedom. The Kyle compatibility
wrapper intentionally preserves Kyle's \code{1.96} CI schema.
Full covariance ordering is covered for dynamic, pretrend, and control terms,
including off-diagonal entries, symmetry/PSD diagnostics, and Stata-style
rank-adjusted joint pretrend tests when the pretrend covariance is singular.
Additional conformance rows remain under active implementation.
}
\value{
A \code{didbjs} S3 object with estimates, controls, covariance, sample mask,
saved artifacts, diagnostics, and the matched call.
}
