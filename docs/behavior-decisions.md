# Behavior Decisions

Status: accepted for conformance profile v1.

These decisions freeze the scope and source-of-truth rules for the first
implementation goal. A future implementation may add features only through a new
conformance profile or a recorded decision update.

## Decision Records

### D001 Package Name And Coexistence

Status: accepted
Affected features: package metadata, namespace, Kyle compatibility
Governing source: CRAN metadata checked 2026-06-19; Kyle package reference
Decision: Use working package name `didbjs` for v1. Do not reuse
`didimputation`, because Kyle's package already owns that name on CRAN. Export a
Kyle compatibility wrapper and migration helpers inside `didbjs` rather than
attempting package-name replacement.
Rejected alternatives: reuse `didimputation`; fork Kyle under the same package
name; use a Stata command name as the R package name.
Required evidence: CRAN name check for `didbjs`; package metadata review;
Kyle-wrapper tests in F040.
Documentation impact: README and package help must say `didbjs` is independent
and compatibility-focused, not a drop-in ownership replacement for Kyle's CRAN
package.
Notes: `Rscript available.packages()` on 2026-06-19 reported
`didbjs_available=TRUE` and `didimputation_available=FALSE`.

### D002 API Architecture

Status: accepted
Affected features: M003, F038, F041, F049
Governing source: R package ergonomics plus Python/Kyle compatibility surfaces
Decision: v1 exposes an R-native S3 API as primary, with explicit compatibility
wrappers/aliases for Python-style and Kyle-style calls. The estimator returns a
stable S3 object with tidy extraction methods; compatibility layers normalize
arguments into the same internal contract.
Rejected alternatives: one overloaded function with implicit mode guessing;
Stata-like syntax as the primary R API; Python object shape as the only R return
shape.
Required evidence: API snapshot tests, object/method tests F049, Python API
mapping F041, Kyle wrapper tests F040.
Documentation impact: docs must separate R-native API, Python-compatible aliases,
and Kyle migration calls.

### D003 License And GPL Boundary

Status: accepted
Affected features: M006, all implementation files, parity fixtures
Governing source: MIT Python/Kyle licenses; GPL-3.0 Stata license
Decision: Target a permissive package license compatible with MIT code reuse and
use a clean-room process for the GPL Stata reference. Stata may be used as a
black-box behavior oracle, help-file/spec source, and output generator; GPL Stata
implementation code must not be translated or adapted into non-GPL R code.
Rejected alternatives: GPL-3.0 package allowing source adaptation; silent
implementation borrowing from Stata; no provenance ledger.
Required evidence: provenance ledger, source-use statement in final report,
pre-release similarity/provenance review.
Documentation impact: include license/provenance section and third-party notices
for any MIT-derived material.

### D004 Default Term Names

Status: accepted
Affected features: term naming, output schema, plotting
Governing source: Stata stored results; Python output object; R API contract
Decision: R-native output uses human-readable term records with canonical `term`
values matching Stata where Stata has a term (`tau`, `tau0`, `pre1`, etc.).
Python aliases (`tau_ate`) are available through Python-compatible extraction.
Kyle-compatible wrapper returns Kyle-style `term`, `estimate`, `std.error`,
`conf.low`, `conf.high`, and optional `lhs` columns.
Rejected alternatives: Python names only; Stata names only; free-form names.
Required evidence: naming snapshot tests across F001-F024, F040, F041, F049.
Documentation impact: term-name table in reference docs.

### D005 Never-Treated Encodings

Status: accepted
Affected features: treatment timing, Kyle wrapper, Python aliases
Governing source: Stata treatment date semantics; Kyle compatibility behavior;
Python input validation
Decision: R-native API treats `NA`, `Inf`, and Stata-style missing imported via
`haven` as never-treated. The Kyle wrapper also treats `0` as never-treated for
compatibility. R-native API rejects `0` treatment timing unless the user enables
Kyle compatibility or explicitly maps it to never-treated. Treatment timing must
be constant within unit after normalizing `NA`/`Inf`/Stata tagged missing values
to the never-treated state. Treatment before the observed sample, treatment
after the observed sample, and negative finite treatment dates are classified by
their event time rather than rejected solely because they are out of sample.
Rejected alternatives: always treat `0` as never-treated; always reject `Inf`;
silent coercion of inconsistent timing.
Required evidence: F026 and F040.
Documentation impact: encoding table and migration note.

### D006 Autosample And Failed Imputation

Status: accepted
Affected features: autosample, diagnostics, sample mask
Governing source: Stata explicit `autosample`; Python automatic drop behavior
Decision: R-native default is fail-closed: non-imputable treated observations
raise an error with diagnostics. `autosample = TRUE` enables Stata-style explicit
dropping/trimming with recorded drop lists and renormalized weights. A
Python-compatible mode may default to Python's automatic autosample but must emit
structured diagnostics.
For F025 sample construction, missing outcome, id, time, controls, fixed-effect
source columns, cluster, analytic weights, and custom treatment-weight columns
are required-data exclusions recorded in the original-row sample mask. Missing
treatment timing is not a required-data exclusion because D005 defines missing
`Ei` as the never-treated encoding. Pinned Python and Kyle references do not
expose a `subset` argument, so their F025 reference jobs prefilter the same rows
before calling the public APIs; the `didbjs` Python and Kyle wrappers may expose
`subset` as an R-side convenience that must be equivalent to that prefilter. For
the static Kyle F025 missingness fixture, the wrapper preserves Kyle's public
estimate and `NA` standard-error shape after the subset prefilter, while the
R-native estimator and covariance remain governed by Stata.
Rejected alternatives: silent automatic drop in R-native mode; no autosample
support.
Required evidence: F013, F025, F032, sample-mask artifacts.
Documentation impact: troubleshooting section for imputation failures.

### D007 No-FE And Constant-Only Semantics

Status: accepted
Affected features: first-stage model, FE parsing
Governing source: Python `fe=None`; Stata `fe()` option behavior; R formula norms
Decision: R-native API represents constant-only model explicitly as `fe = NULL`.
Default is unit and time FE. Unit-only, period-only, and arbitrary FE lists are
explicit. Empty character vectors are invalid to avoid confusing no-FE with a
missing argument.
Rejected alternatives: treating omitted `fe` as no FE; using formula magic only.
Required evidence: F008, F031, F038.
Documentation impact: first-stage model examples.

### D008 Weight Semantics

Status: accepted
Affected features: analytic weights, custom estimands, variance
Governing source: Stata weight options and Python `aw` behavior
Decision: v1 supports analytic weights for first-stage fitting and estimand
aggregation. Importance/frequency weights are excluded from v1 unless a later
profile adds them. Weight scaling must be invariant where the reference
semantics imply normalization; negative custom `wtr` is allowed only for
`sum = TRUE` difference estimands.
Rejected alternatives: support all Stata weight classes in v1; silently treat
all weights as analytic.
Required evidence: F005, F006, F007, F028, F029, F037.
Documentation impact: weight-class limitations and normalization rules.

### D009 Finite-Sample And Cluster Rules

Status: accepted
Affected features: standard errors, covariance, CI
Governing source: Stata output for finite-sample behavior plus algebraic oracle
Decision: Stata governs v1 clustered covariance, finite-sample correction,
`avgeffectsby`, and small-cohort warning semantics. R implementation must expose
full covariance names/order and exact sample masks. One-cluster and singleton
cluster cases must fail with structured errors unless Stata provides a defined
result in the pinned fixture.
Rejected alternatives: use `fixest` defaults without parity; ignore
off-diagonal covariance; report SEs without `V` parity.
Required evidence: F001, F015-F017, F033-F036.
Documentation impact: inference section and warning classes.

### D010 `minn` Behavior

Status: accepted
Affected features: low support suppression, output schema
Governing source: Stata `minn` behavior and Python implementation notes
Decision: Suppressed estimands are retained as explicit rows with missing
estimate/SE and a structured suppression diagnostic unless Stata requires
zeroing for a specific compatibility extractor. Internal weights for suppressed
terms must be zeroed or omitted exactly as required by the chosen reference and
recorded in artifact metadata.
Rejected alternatives: silent omission; returning zero as an estimate in
R-native output.
Required evidence: F014, F035, plot handling in F022-F024.
Documentation impact: suppression diagnostics section.

### D011 Saved Artifacts And Row Identity

Status: accepted
Affected features: saveweights, loadweights, saveestimates, saveresid
Governing source: Stata saved variables; Python output object; R object norms
Decision: R-native API returns saved artifacts inside the result object with
stable original row IDs. Optional helper functions can materialize columns into a
copy of the input data. The package must not mutate user input by default. If
the input has a unique `row_id` column, that column is intentionally adopted as
the canonical artifact key; otherwise `didbjs` generates positional row IDs from
the original input order. Duplicate user-supplied `row_id` values fail closed.
`loadweights` artifacts include schema version, source fixture/spec hash, and
outcome compatibility checks. Bare data frames with `row_id`, `term`, and
`weight` are accepted only as a manual override: they cannot carry `spec_hash`
metadata, so `didbjs` signals class `didbjs_manual_loadweights_warning` and
still requires complete row/term coverage, known rows and terms, no duplicate
pairs, and finite weights. Valid manual loads continue under `warn >= 2` after
signalling that classed condition. For F039, Stata
saved variables govern numeric saved-weight values and loadweights reuse, while
R object norms govern stable row-ID keyed dense/sparse artifacts and
serialization. The pinned Python reference governs the presence and names of
the `weights` object fields; its F039 reordered dynamic saveweights probe
records upstream row-order drift and is not used as the row-identity oracle.
Rejected alternatives: mutate input data by default; store weights without row
identity; silently accept unkeyed or partial manual weights; allow weight reuse
across incompatible samples.
Required evidence: F018, F019, F039.
Documentation impact: artifact extraction and serialization docs.

### D012 Python Placeholder Arguments

Status: accepted
Affected features: Python compatibility
Governing source: pinned Python signature and README limitations
Decision: Python arguments present but intentionally unimplemented upstream are
classified as placeholder API. R-native implementation is not required to support
them as Python-compatible features in v1 unless Stata/profile rows make them
mandatory. Passing such arguments in Python-compatible mode must produce a clear
`unsupported-by-design` error only if preapproved in the conformance profile.
Rejected alternatives: treat Python placeholders as supported Python behavior;
silently ignore placeholders.
Required evidence: F041.
Documentation impact: Python compatibility matrix.

### D013 `event_plot` Semantic Parity

Status: accepted
Affected features: event_plot, plotting validation
Governing source: Python `event_plot`; Stata `event_plot` extraction/stub rules;
R plotting norms
Decision: v1 implements semantic plotting parity: coefficient extraction,
pre/effect ordering, CI coordinates, trimming, shifting, perturbation, overlays,
manual inputs, `noplot`/plot-data extraction, and save behavior. Literal Stata
graph-option syntax is excluded from v1; equivalent R styling arguments and plot
object customization are required.
Stata reference regeneration for direct matrix inputs may seed a harmless active
e-class result and a neutral `coef` variable before calling the pinned ado:
Stata 14.2 evaluates `e(cmd)` while resolving explicit stubs and later reaches a
graph-prep expression that expands to `mi(coef)` even under `savecoef noplot`.
This harness does not change the coefficient or variance matrices being
extracted.
Rejected alternatives: literal Stata graph syntax; plot-data only with no
rendering; rendering without extractable data.
Required evidence: F022-F024, F042-F044.
Documentation impact: plot translation table and examples.

### D014 Kyle Multi-Outcome Compatibility

Status: accepted
Affected features: Kyle wrapper, multiple outcomes
Governing source: Kyle `didimputation` public API
Decision: Kyle multiple-LHS behavior is mandatory for the Kyle compatibility
wrapper. R-native API may expose a clearer multi-outcome interface, but the
wrapper must accept Kyle-style calls and return Kyle-style `lhs` output.
Rejected alternatives: prose-only migration; dropping multi-outcome behavior.
Required evidence: F021 and F040.
Documentation impact: Kyle migration vignette.

### D015 Release Target And Supported Matrix

Status: accepted
Affected features: package checks, CI, examples
Governing source: CRAN-quality R package standards and project quality bar
Decision: v1 is held to CRAN-quality gates even before CRAN submission. Support
R 4.4 release as the development baseline, R oldrel/release/devel in CI, and
Linux/macOS/Windows for package checks. Default tests must run offline without
Stata, Python, SSH, or user-specific paths.
Rejected alternatives: local macOS-only package check; optional `--as-cran`;
network-dependent default tests.
Required evidence: F050, package check logs, CI matrix.
Documentation impact: installation, test, and release docs.

### D016 Kyle `idname = "i"` Compatibility Divergence

Status: accepted
Affected features: F001, F040, Kyle compatibility wrapper
Governing source: pinned Kyle `didimputation` commit
`69b4f8dfe16b007474721fc5610859b56a80cdc6`; F001 Kyle alias probe
Decision: `didbjs` must accept Kyle-style calls whose unit id column is
literally named `i`. The pinned Kyle package errors for F001 because its
variance helper uses a loop variable named `i` inside `data.table` NSE, causing
`v_star[, i]` to resolve `i` as the data column rather than the loop counter.
For this collision, the normative Kyle comparison is the committed alias probe
that renames the unit/cluster column to `unit_id` while preserving rows,
outcome, time, treatment timing, and weights. The exact failing Kyle call
remains committed as `reference_error` evidence; the `didbjs` wrapper returns
the alias-probe public output.
Rejected alternatives: reproduce Kyle's NSE error in `didbjs`; mutate the
upstream Kyle reference; silently ignore the divergence; loosen F001 numeric
tolerances.
Required evidence: F001 Kyle diagnostics with root cause, alias-probe artifacts,
and `did_imputation_kyle()` regression tests comparing against the alias probe.
Documentation impact: Kyle wrapper help must describe the `idname = "i"`
divergence and alias-probe basis.

### D017 Python Fixed-Effect Numerical Divergence

Status: accepted
Affected features: F001, F002, F003, F004, F005, F007, F009, F027, F036, F041, Python compatibility wrapper
Governing source: pinned Python `did_imputation` commit
`c7765a9fb2dcc48dc745b356784b4e9ce8b1d376`; F001-F005, F007, F009, and F027 Python diagnostics;
Stata/algebraic/Kyle agreement
Decision: For fixed-effect fixtures F001-F005, F007, F009, and F027, the pinned Python reference
is normative for API argument names, object field names, object nullability,
warning capture, and Python-compatible schema, but not for core point estimates
under TOL001 when Stata, algebraic fixtures, and Kyle-compatible output agree.
In F001 the Python public call returns `tau_ate = 2.0000003632157988`, outside
the frozen `1e-10` scalar tolerance, while Stata, the hand/algebraic oracle, and
the Kyle alias probe agree on `2`. In F002 the Python public call returns each
dynamic horizon with the same `3.632157987709661e-7` drift from the algebraic
targets `tau0 = 1`, `tau1 = 2`, and `tau2 = 3`; Stata and Kyle agree with the
algebraic targets and Stata governs the full covariance matrix. F003's Python
`allhorizons=True` path discovers the same public terms as Stata, but the
discovered point estimates retain the same drift as F002. `didbjs` must return
the Stata/algebraic/Kyle core estimates in both R-native and Python-compatible
wrappers, while retaining committed Python drift as diagnostic evidence and
preserving Python object shape, including dynamic, all-horizon, and hbalance
field names and hbalance unit selection. F005's Python custom positive `wtr`
path preserves raw custom-weight estimate names, but its two point estimates
both drift by `8.286954762404264e-7`; Stata and the algebraic normalized-weight
targets agree on `tau_wtr_uniform = 2.5` and
`tau_wtr_late = 2.833333333333333`. F007's analytic-weight path preserves the
Python object shape and proves weights change the estimate, but the pinned
Python point estimate drifts from Stata by about `3.863053e-7`. F009's Python
control path preserves the expected non-null control fields and collinearity
error shape, but its point estimate and control standard errors differ slightly
from the Stata-governed estimator and covariance. F027's irregular/unbalanced
panel preserves the expected Python object shape, but the Python public point
estimate drifts from the Stata/algebraic target by about `7.2e-7`.
Rejected alternatives: loosen TOL001; copy Python's iterative fixed-effect
drift into the R implementation; change the frozen F001/F002 input fixtures;
treat the Python public scalars as the estimator oracle when
Stata/algebraic/Kyle agree.
Required evidence: Python diagnostics with `tol001_pass = false`, F001-F005, F007, F009, and F027
tests asserting the drift magnitude, and `did_imputation_python()` tests
matching the pinned Python object schema while using the core Stata/algebraic
estimates.
Documentation impact: Python wrapper help must describe that Python numeric
divergence is approved for the recorded fixed-effect fixtures while
Python-compatible object shape is preserved.

### D018 Kyle Analytic-Weight SE Divergence

Status: accepted
Affected features: F007, F040, Kyle compatibility wrapper
Governing source: pinned Kyle `didimputation` 0.5.0; Stata `did_imputation`
14.2 reference artifacts; D009 Stata covariance rule
Decision: For analytic-weight fixtures where Kyle and Stata agree on the
weighted point estimate but not on clustered standard errors, Stata governs the
R-native covariance and the Kyle-compatible wrapper's numeric SE. The Kyle
wrapper preserves Kyle's public output shape and term naming, but it does not
copy Kyle's analytic-weight SE when it disagrees with the Stata covariance
oracle.
Rejected alternatives: copy Kyle's weighted SE into the wrapper; loosen the
Stata covariance tolerance; treat F007 as Kyle-only for inference.
Required evidence: F007 Kyle diagnostics and estimates showing point-estimate
agreement with Stata and `std.error` divergence; F007 wrapper tests comparing
the wrapper estimate to Kyle and the wrapper SE to Stata.
Documentation impact: Kyle compatibility docs must distinguish output-shape
compatibility from inference-source compatibility for analytic weights.

### D019 Stata 14 `hetby` Runtime Divergence

Status: accepted
Affected features: F020, heterogeneity by subgroup
Governing source: pinned Stata `did_imputation` source at commit
`767c8d6670a751170910d419bbafd323df92ef08`; controlled licensed Stata host Stata 14.2
runtime artifacts
Decision: `didbjs` implements the source-level `hetby` semantics from the
pinned Stata ado: treated subgroup values are validated as non-negative
integers, split into one estimand per subgroup, normalized within subgroup for
average estimands, and named as `<base>_<level>`. On the controlled Stata 14.2
host, the direct command `hetby(group)` is unreachable for F020 because the
pinned ado checks `r(r)>30` after `levelsof`, while this runtime returns
`r(levels)` but not `r(r)`. Since Stata missing values compare greater than 30,
the direct command fails with return code 149 and the message
`The hetby variable takes too many (over 30) values` even for two subgroups.
F020 therefore records the direct failure as approved-divergence evidence and
uses the equivalent Stata split-weight oracle `wtr(g0 g1)`, with terms renamed
from `tau_g0`/`tau_g1` to `tau_0`/`tau_1`, for point estimates, covariance, and
sample-mask parity.
Rejected alternatives: treat F020 as blocked; silently patch the pinned Stata
ado; reproduce the Stata 14 runtime failure in `didbjs`; loosen F020
tolerances.
Required evidence: F020 `direct-hetby-error.json`, equivalent split-`wtr`
Stata artifacts, and F020 regression tests for direct `hetby` semantics plus
invalid combinations.
Documentation impact: F020 fixture docs and progress notes must distinguish the
direct Stata runtime divergence from the supported R `hetby` semantics.

### D020 Irregular Panel Duplicate Guard

Status: accepted
Affected features: F027, F040, row identity, Kyle compatibility wrapper
Governing source: F027 Stata, Python, and Kyle duplicate probes; D011 row
identity rule; D006 sample-mask diagnostics
Decision: `didbjs` rejects duplicate `(unit, time)` rows after applying explicit
subset and missing-required filters. The pinned Stata, Python, and Kyle
references all accept the F027 duplicate probe and return an estimate, but v1
requires original-row sample masks, saved artifacts, and loadweights metadata to
have unambiguous observation identity. Duplicate unit-time rows therefore fail
closed in R-native and compatibility-wrapper calls with a structured
`didbjs_contract_error` naming the duplicate row ids. For the positive F027
irregular/unbalanced fixture, Stata plus the algebraic average treatment effect
govern point estimates and covariance; Python and Kyle references govern API and
output-shape compatibility. The Stata public point estimate on this fixture has
a tiny solver offset from the algebraic target, below `TOL002`; `didbjs` uses
the algebraic/Stata agreement for the core estimate, records the Python drift
diagnostic, and records exact Kyle output plus duplicate-probe evidence.
Rejected alternatives: silently accept duplicate unit-time rows; aggregate
duplicates; reproduce reference duplicate acceptance; loosen row-identity
requirements for saved artifacts.
Required evidence: F027 duplicate-probe artifacts for Stata, Python, and Kyle;
F027 regression tests for the duplicate error class and row ids; positive F027
parity tests for irregular gaps, nonconsecutive periods, unequal panel lengths,
sample masks, and wrapper output shapes.
Documentation impact: package help must document duplicate unit-time rejection
as an input validation rule.

### D021 Positive-Finite Analytic Weight Guard

Status: accepted
Affected features: F028, analytic weights, Kyle compatibility wrapper, Python
compatibility wrapper
Governing source: F028 Stata, Python, and Kyle scaling and invalid-weight
probes; D008 analytic-weight scope
Decision: `didbjs` treats analytic weights as a strict positive finite contract
after subset and missing-required filters. Missing analytic weights are excluded
from the estimation sample and recorded in `sample_mask`, matching the F028
Stata missing-weight sample-mask probe. Zero, negative, infinite, and all-zero
analytic weights fail closed in R-native and compatibility-wrapper calls with a
structured `didbjs_contract_error`. The pinned Stata reference accepts a single
zero analytic-weight row, while the pinned Python and Kyle references accept
single zero and negative analytic-weight rows and Kyle returns `NaN` for an
infinite-weight probe. RC-v1 rejects those permissive cases to preserve a single
positive-finite analytic-weight invariant across estimator, saved-artifact, and
compatibility paths. Non-analytic Stata-style weight classes remain unsupported:
Stata rejects `iweight` and `fweight`, and `didbjs` rejects corresponding
placeholder arguments as unsupported.
Rejected alternatives: reproduce upstream zero/negative-weight permissiveness;
silently coerce invalid weights to missing; add importance or frequency weights
to v1.
Required evidence: F028 base/scaled Stata, Python, and Kyle artifacts; F028
missing-weight sample-mask artifact; F028 invalid-weight probes; F028 regression
tests for strict R-native, Python-wrapper, and Kyle-wrapper errors.
Documentation impact: package help must say analytic weights are missing-dropped
but otherwise strictly positive and finite, and non-analytic weight classes are
excluded from v1.

### D022 Custom Wtr Stata Runtime Offset

Status: accepted
Affected features: F029, custom estimand weights, Stata parity
Governing source: F029 Stata artifacts, F005/F006 custom-weight algebraic
oracles, D008 custom-weight semantics
Decision: F029 uses Stata as the validation oracle for custom `wtr` edge cases
and preserves exact row masks, term names, duplicate-name errors, negative
non-`sum` errors, zero-weight omitted output, and untreated-support behavior.
For the F029 multiple-`wtr` and missing-`wtr` scenarios, the pinned Stata 14.2
runtime returns point estimates about `4.2e-9` above the algebraic/R value even
on an exactly additive fixture. The offset is stable, below `TOL002`, and does
not affect the validation semantics being tested by F029. F029 therefore uses
`1e-8` for those recorded Stata runtime point-estimate comparisons while
retaining exact checks for masks, row ids, terms, error classes, and option
support. Base/scaled and untreated-support invariance remain checked at
stricter scalar tolerance.
Rejected alternatives: loosen all custom-weight point-estimate tolerances;
perturb the R estimator to reproduce Stata's multi-`wtr` numerical offset;
drop the missing-`wtr` or multiple-`wtr` probes from F029.
Required evidence: F029 Stata diagnostics showing base/scaled and
untreated-support differences below `1e-12`, F029 estimates showing the
multiple/missing Stata runtime values, and F029 regression tests using the
narrow `1e-8` tolerance only for those Stata-offset comparisons.
Documentation impact: progress notes must identify F029 as parity-verified with
a recorded Stata runtime numerical offset.

### D023 Horizon Boundary Fail-Closed Policy

Status: accepted
Affected features: F030, dynamic horizons, Python compatibility wrapper, Kyle
compatibility wrapper
Governing source: F030 Stata, Python, and Kyle horizon-boundary probes; D004
term naming; D006 fail-closed diagnostics
Decision: `didbjs` accepts unsorted and sparse non-negative integer horizon
vectors, preserving requested Stata/Python order in R-native and
Python-compatible output while returning Kyle-compatible dynamic rows in Kyle's
sorted term order. Duplicate, negative, absent, and empty R-native horizon
requests fail closed with structured `didbjs_contract_error` conditions.
`horizons` and `allhorizons` cannot be combined. The Python-compatible wrapper
continues to treat an empty `horizons` vector as the omitted Python default for
static ATT, but rejects negative, duplicate, and absent explicit horizons before
they can produce Python's permissive zero, infinite-SE, or duplicated-label
behavior. The Kyle-compatible wrapper rejects empty explicit horizons and
`horizon = TRUE` for RC-v1 instead of reproducing Kyle's vacuous
`integer(0)`/all-horizon behavior.
Rejected alternatives: reproduce Stata/Python absent-horizon zero rows; allow
Python negative-horizon `tau-1` output with infinite standard error; reproduce
Kyle's `integer(0)` all-horizon behavior; silently sort R-native/Python
horizons.
Required evidence: F030 reference probes for Stata, Python, and Kyle; F030
regression tests for unsorted/sparse parity, absent/empty fail-closed behavior,
structured invalid errors, and compatibility-wrapper order.
Documentation impact: estimator and wrapper help must describe non-negative
integer horizon validation, explicit empty-horizon behavior, and
`allhorizons`/`horizons` exclusivity.

### D024 FE Rank And Absorbed-Control Fail-Closed Policy

Status: accepted
Affected features: F031, first-stage fixed effects, controls, covariance
Governing source: F031 Stata and Python rank-pathology probes; D006
fail-closed diagnostics; D007 explicit FE semantics
Decision: `didbjs` follows Stata for successful rank-deficient FE designs:
nested FE, duplicate FE, singleton FE levels, and disconnected FE components
must keep Stata's estimates, covariance shape, and sample masks after dropping
only redundant first-stage design columns for covariance solving. Tiny
near-singular covariance differences in the singleton/disconnected probes are
classified under TOL006 with a frozen `1e-6` matrix/SE tolerance and
`1e-7` estimate tolerance. Sparse first-stage designs use dense QR for small
rank-resolution fallbacks up to `5e6` matrix entries to preserve the v1 parity
surface; larger sparse rank-deficient fallbacks use sparse QR when it can select
an independent column set, and otherwise fail closed rather than densifying a
potentially oversized design matrix. For controls absorbed by fixed effects,
Stata emits a successful result with an omitted zero control row, while Python
raises a collinearity error. `didbjs` uses the Python-style fail-closed behavior for
absorbed controls and untreated-sample-only rank failures to avoid reporting
unidentified control coefficients as meaningful estimates.
Rejected alternatives: silently report Stata's omitted-zero absorbed-control
row; reject all redundant FE designs; treat `fe = character()` as no fixed
effects.
Required evidence: F031 Stata/Python probes, F031 covariance/sample-mask
artifacts, and regression tests for successful FE rank cases plus structured
absorbed-control and untreated-rank errors.
Documentation impact: estimator and Python-wrapper help must describe
rank-deficient FE support and fail-closed absorbed-control handling.

### D025 Identification Failure Fail-Closed Policy

Status: accepted
Affected features: F032, first-stage identification, autosample diagnostics,
Python compatibility wrapper
Governing source: F032 Stata and Python identification-failure probes; D006
autosample diagnostics; D023 absent-horizon fail-closed policy
Decision: `didbjs` fails closed for designs with no untreated observations, no
treated observations, all units entering one cohort with unsupported post
periods, static designs where all post periods requiring imputation are treated,
and requested post-treatment horizons with zero treated support. Stata records
omitted zero rows for no-treated and unsupported-horizon probes, and Python can
record a zero unsupported-horizon row; these are treated as divergence evidence
rather than valid identified estimates. The Python-compatible wrapper may retain
dynamic autosample behavior covered by F013, but static ATT calls that autosample
away or trim the requested static estimand raise structured
`didbjs_contract_error` conditions.
Rejected alternatives: report omitted zero estimates for unidentified designs;
allow static Python-wrapper autosample to silently change the estimand; surface
raw `fixest` errors for no-untreated first stages.
Required evidence: F032 Stata/Python probes, omitted-zero estimate artifacts,
cannot-impute row artifacts, and regression tests for R-native and
Python-compatible structured errors.
Documentation impact: estimator and wrapper help must describe fail-closed
identification boundaries.

### D026 F046 Stata Batch Numerical Floor

Status: accepted
Affected features: F046, randomized differential testing
Governing source: pinned Stata `did_imputation` commit
`767c8d6670a751170910d419bbafd323df92ef08`; F046 Stata batch artifacts
Decision: F046 keeps Stata as the governing core oracle, but the committed
batch reference artifacts record a Stata numerical floor of about `4e-8` across
duplicated dynamic seed geometries that are algebraically equivalent to F002.
The F046 randomized differential test therefore uses a narrow `1e-7`
artifact-parity threshold for Stata batch point estimates while preserving exact
term, row, sample-mask, status, and failure-file checks. This is not a tolerance
change for the single-fixture F001/F002 algebraic oracles, and it does not make
Python or Kyle the core estimator oracle.
Rejected alternatives: generate F046 expected estimates from `didbjs`; drop
late-seed cases until the artifact appears TOL001-clean; treat the Python or
Kyle references as the core oracle.
Required evidence: F046 diagnostics and progress notes must record the maximum
observed Stata batch drift, the controlled Stata host run, and zero retained
minimal failures.
Documentation impact: F046 progress notes must distinguish the Stata batch
artifact floor from Python D017 drift and Kyle compatibility output shape.

### D027 Kyle Critical-Value Compatibility Surface

Status: accepted
Affected features: F015, F034, F040, Kyle compatibility wrapper
Governing source: pinned Kyle `didimputation` 0.5.0 public output schema; D009
Stata CI semantics for the R-native estimator
Decision: R-native estimates, controls, and pretrend rows use
`significance_level` and Stata's normal critical-value semantics. The
Kyle-compatible wrapper does not expose `significance_level` and intentionally
preserves Kyle's public `estimate +/- 1.96 * std.error` confidence interval
schema for ATT, dynamic, and Kyle pretrend rows. This is an output-surface
compatibility rule, not a change to the Stata-governed covariance oracle.
Rejected alternatives: add a Kyle-wrapper-only `significance_level`; use
`qnorm(0.975)` in Kyle-compatible output; let native and Kyle pretrend CIs
silently differ without documentation.
Required evidence: F015 tests for native pretrend alpha behavior and Kyle
pretrend `1.96` rows; F034 tests for R-native Stata CI semantics and Kyle static
`1.96` schema; F040 tests for Kyle public-call CI arithmetic.
Documentation impact: Kyle compatibility help must state that all wrapper CIs,
including pretrend rows, use Kyle's `1.96` convention.
