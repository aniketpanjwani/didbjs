# Exhaustive Verification Criteria

This is the acceptance surface for a complete R package. A future implementation
goal should point here and require evidence for each relevant section.

Use this matrix together with `docs/parity-verification-playbook.md`. This file
defines what must be covered; the playbook defines how the evidence must be
generated, stored, tolerated, and reported.

## Verification Levels

Use these statuses for every feature:

- `parity-verified`: compared against Python, Stata, Kyle R, or algebraic
  fixture outputs.
- `implemented-only`: works and is covered by automated tests, but no external
  reference exists.
- `approved-divergence`: intentionally differs from a reference and has a
  pre-existing decision record, user-facing documentation, and regression test.
- `unsupported-by-design`: rejected with a clear user-facing error.
- `blocked`: cannot be verified yet; blocker and unlock condition are recorded.

Mandatory features may finish only as `parity-verified` or
`approved-divergence`. `blocked` always means `terminated-incomplete`.

## Source Of Truth Rules

Default hierarchy:

1. Stata `borusyak/did_imputation` for canonical estimator semantics.
2. Python `gmarinichev/did_imputation` for the announced Python API and implementation choices.
3. Kyle Butts `didimputation` for existing R user ergonomics and compatibility.

If references disagree:

- record the exact fixture/specification;
- record Stata output if available;
- record Python output if available;
- state which behavior the R package follows;
- add a regression test for the chosen behavior.

## Feature Matrix To Complete

Estimator inputs:

- `data`/`df`
- outcome variable
- unit id
- time variable
- treatment timing variable
- sample/subset equivalent
- analytic/frequency/import weights decision
- missing value handling
- integer time and `delta` validation
- treatment date encodings: missing, `NA`, zero, `Inf`

Model of untreated potential outcomes:

- default unit/time FE
- no FE / constant-only model
- period-only FE
- unit-only FE
- arbitrary FE list
- interacted/discrete FE equivalent to Stata `#`
- continuous controls
- unit-interacted controls
- time-interacted controls
- collinearity checks in untreated versus full sample
- repeated cross-section specification
- triple-difference specification

Estimands:

- default static ATT
- custom `wtr`
- multiple `wtr`
- weighted averages
- weighted sums
- negative `wtr` only with `sum`
- horizons
- all horizons
- horizon balance
- `minn` suppression
- heterogeneity by subgroup (`hetby`)
- projection of treatment effects (`project`)
- `shift`
- autosample / cannot-impute handling

Standard errors:

- default cluster by unit
- custom cluster variable
- no standard errors (`nose`)
- default `avgeffectsby = Ei, t`
- custom `avgeffectsby`
- small-cohort warnings
- leave-out SEs
- tolerance and max-iteration behavior
- convergence diagnostics
- full variance-covariance matrix naming and ordering
- controls included in covariance matrix
- pretrends included in covariance matrix

Saved artifacts:

- imputed treatment effects per observation
- imputation weights
- load/reuse weights for another outcome
- residuals used for SE computation
- `N`, `Nc`, `Nt`
- suppressed coefficient list
- autosample drop/trim lists
- iteration count

Pretrends:

- no pretrend by default
- `pretrends = k` creates `pre1...prek`
- pretrend regression uses untreated observations only
- post-treatment effects do not change when pretrends are added
- reference group is all periods before `-k` plus never-treated observations
- individual pretrend SEs
- joint F statistic
- joint p value
- degrees of freedom

Output:

- stable class
- stable data frame/tidy output
- stable print method
- stable summary method
- `coef`, `vcov`, `tidy`, and possibly `glance` methods
- Python-compatible names when requested
- Kyle-compatible output or wrapper when requested
- exact naming of `tau`, `tau_ate`, `tau0`, custom weights, heterogeneity, projections, controls

Plotting:

- result-object input
- manual coefficient/SE input
- pretrend and effect sorting
- CI calculation from configurable alpha/significance level
- `rcap` equivalent
- `rarea` equivalent
- `together = TRUE/FALSE`
- no-SE behavior
- zero line
- event line
- labels/title/theme controls
- legend behavior
- save path
- plot data extraction for tests
- Stata-like stubs for lag/lead names
- trim lag/lead
- shift
- multiple model overlays
- perturbation
- per-model style options
- save coefficient data

Error and warning behavior:

- invalid option combinations
- no untreated observations
- non-integer relative time
- incompatible `delta`
- `horizons` plus `allhorizons`
- `wtr` plus horizons when unsupported
- negative weights without `sum`
- autosample with incompatible options
- project/hetby incompatibilities
- failed imputation
- unsupported Stata/Python features
- missing required columns
- all-zero weights
- low effective number warning

Performance:

- small panel smoke benchmarks
- medium panel correctness benchmarks
- large panel stress benchmark
- many horizons benchmark
- many fixed effects benchmark
- controls benchmark
- weights benchmark
- memory allocation tracking
- comparison to Kyle R package on common fixtures
- comparison to Python on all overlapping benchmark features in the frozen
  conformance profile

Packaging:

- `R CMD check`
- `R CMD check --as-cran` if CRAN is a target
- `testthat`
- docs generated from roxygen
- examples run
- README examples run
- vignettes build
- no hidden internet dependency in tests
- no reliance on `<reference-clone-root>` for package tests unless explicitly marked integration
- license decision documented

## Fixture Suite

### F001 Static Balanced Panel

Purpose:

- default ATT, unit/time FE, unit clustering.

Checks:

- point estimate;
- SE;
- `V`;
- `N`, `Nc`, `Nt`;
- R/Python/Stata parity.

### F002 Dynamic Horizons

Purpose:

- `horizons = 0:K`, coefficient naming, dynamic ATT.

Checks:

- all requested horizons;
- missing horizons behavior;
- sorted output;
- plot data.

### F003 All Horizons

Purpose:

- discover all non-negative horizons from the treated sample.

Checks:

- same horizon set as references;
- no pretrend leakage;
- output ordering.

### F004 Horizon Balance

Purpose:

- only units with all chosen horizons contribute to each estimand.

Checks:

- balanced unit set;
- point estimates;
- incompatible weights errors.

### F005 Custom Positive Weights

Purpose:

- user-defined estimand through `wtr`.

Checks:

- normalization;
- weighted effect;
- multiple `wtr` covariance.

### F006 Difference Estimand With Negative Weights

Purpose:

- `sum = TRUE`, negative `wtr`, difference between horizons/groups.

Checks:

- negative weights rejected without `sum`;
- accepted with `sum`;
- estimate/SE parity.

### F007 Analytic Weights

Purpose:

- weights affect first-stage regression and estimand aggregation.

Checks:

- weight normalization;
- weighted first-stage fit;
- weighted ATT.

### F008 Alternative FE Sets

Purpose:

- no FE, period-only FE, arbitrary FE list.

Checks:

- FE semantics;
- imputation feasibility;
- output parity.

### F009 Continuous Controls

Purpose:

- controls in first-stage untreated regression and reported control coefficients.

Checks:

- control estimates;
- control SEs;
- covariance matrix with controls;
- collinearity handling.

### F010 Repeated Cross Section

Purpose:

- unit id differs from clustering/group FE.

Checks:

- FE/cluster behavior;
- treatment timing semantics.

### F011 Triple Difference

Purpose:

- composite unit id and interacted fixed effects.

Checks:

- composite ID handling;
- FE interactions;
- cluster at broader unit.

### F012 Shift And Delta

Purpose:

- anticipation shift and non-unit time step.

Checks:

- relative-time labels;
- treatment classification;
- invalid delta errors.

### F013 Failed Imputation / Autosample

Purpose:

- observations for which FE cannot be imputed.

Checks:

- explicit error or autosample behavior;
- cannot-impute marker;
- drop/trim diagnostics;
- renormalized weights.

### F014 `minn` Suppression

Purpose:

- low effective treated observation count.

Checks:

- suppression list;
- warning text;
- coefficient zeroing or omission semantics.

### F015 Pretrends

Purpose:

- separate untreated-only pretrend regression.

Checks:

- `pre1...prek`;
- post-treatment estimates unchanged;
- SEs;
- joint test when included in the frozen conformance profile.

### F016 Small Cohorts And `avgeffectsby`

Purpose:

- SE behavior under small cohorts.

Checks:

- warning behavior;
- default versus coarser `avgeffectsby`;
- SE parity.

### F017 Leave-Out SE

Purpose:

- Stata recommended `leaveout` behavior.

Checks:

- successful leave-out case;
- singleton/cohort failure case;
- exact error guidance.

### F018 Save/Load Weights

Purpose:

- estimator as weighted sum of outcomes and reuse for second outcome.

Checks:

- weights reproduce estimates;
- unit/time sums;
- loadweights parity.

### F019 Save Estimates And Residuals

Purpose:

- observation-level treatment effects and SE residual components.

Checks:

- saved columns;
- missingness;
- residual formulas.

### F020 Heterogeneity And Projection

Purpose:

- `hetby` and `project`.

Checks:

- names;
- subgroup estimates;
- projection coefficients;
- invalid combinations.

### F021 Multiple Outcomes

Purpose:

- Kyle-compatible multi-LHS behavior when included in the frozen conformance profile.

Checks:

- `lhs` output;
- repeated parity for each outcome;
- no cross-outcome contamination.

### F022 Event Plot Basic

Purpose:

- Python-compatible `event_plot`.

Checks:

- `rcap`;
- `rarea`;
- manual input;
- object input;
- saved output;
- plot-data test.

### F023 Event Plot Stata-Like

Purpose:

- multi-model/stub/trim/shift/perturb behavior.

Checks:

- coefficient extraction;
- overlay positions;
- CI data;
- saved coefficient data.

### F024 Performance Small/Medium/Large

Purpose:

- address "limited computational efficiency" concern.

Checks:

- benchmark rows/units/periods;
- memory use;
- Kyle comparison;
- Python comparison on all overlapping benchmark features in the frozen
  conformance profile.

### F025 Missingness And Subset Semantics

Purpose:

- exact sample construction under missing values and subset filters.

Checks:

- missing outcome, controls, FE, cluster, treatment timing, and weights;
- subset handling;
- exact sample mask;
- `N`, `Nc`, `Nt`.

### F026 Treatment-Timing Encodings

Purpose:

- normalize never-treated and out-of-sample treatment encodings.

Checks:

- `NA`, zero, `Inf`, Stata missing, treatment before sample, treatment after
  sample, inconsistent within-unit treatment dates, and negative dates.

### F027 Irregular And Unbalanced Panels

Purpose:

- applied-data panel pathologies beyond clean balanced panels.

Checks:

- gaps in time;
- nonconsecutive periods;
- unequal panel lengths;
- duplicate unit-time rows;
- repeated observations.

### F028 Weight Type And Scaling

Purpose:

- weight-class and scaling semantics.

Checks:

- analytic versus importance weight decision;
- multiplication by a constant;
- zero, missing, infinite, and negative weights;
- all-zero sample behavior.

### F029 Custom `wtr` Validation

Purpose:

- robust validation for custom estimand weights.

Checks:

- multiple weights;
- duplicate names;
- zero-sum weights;
- missing values;
- nonzero support on untreated observations;
- arbitrary scaling.

### F030 Horizon Input Boundaries

Purpose:

- validate horizon input edge cases.

Checks:

- unsorted, duplicated, absent, sparse, and negative horizons;
- empty requested horizons;
- `horizons` plus `allhorizons`.

### F031 FE Rank Pathologies

Purpose:

- fixed-effect and control rank failures.

Checks:

- singleton FE levels;
- disconnected FE components;
- nested FE;
- perfect collinearity;
- absorbed controls;
- untreated-sample-only rank failure.

### F032 Identification Failures

Purpose:

- fail honestly when the design cannot identify requested effects.

Checks:

- no untreated observations;
- no treated observations;
- all units in one cohort;
- all post periods treated;
- no supported post-treatment horizon.

### F033 Cluster And Finite-Sample Rules

Purpose:

- cluster and finite-sample inference boundaries.

Checks:

- one cluster;
- two clusters;
- singleton clusters;
- missing cluster IDs;
- nested clusters;
- alternative-cluster ordering.

### F034 CI And Degrees-Of-Freedom Parity

Purpose:

- confidence interval and critical-value semantics.

Checks:

- normal versus t critical values;
- alpha boundaries;
- finite-sample multiplier;
- exact CI coordinates.

### F035 Covariance Ordering And Singularity

Purpose:

- full covariance matrix contract.

Checks:

- full off-diagonal `V`;
- matrix symmetry;
- naming and ordering;
- PSD diagnostics;
- singular pretrend covariance;
- joint-test failure semantics.

### F036 Hand-Solvable Algebraic Oracle

Purpose:

- independent statistical oracle not copied from any reference implementation.

Checks:

- tiny panels with hand-derived estimates;
- hand-derived weights where possible;
- agreement with Stata/Python/Kyle only after independent derivation is fixed.

### F037 Transformation Invariance

Purpose:

- invariants that should hold regardless of implementation details.

Checks:

- row permutation;
- unit relabeling;
- time translation;
- outcome scaling;
- constant shifts under appropriate FE;
- weight rescaling.

### F038 Data Type And Object Behavior

Purpose:

- R data container and type ergonomics.

Checks:

- integer, double, factor, and character IDs;
- tibbles and data tables;
- nonsyntactic names;
- row names;
- input immutability.

### F039 Saved-Artifact Row Mapping

Purpose:

- saved outputs remain tied to original observations.

Checks:

- original row IDs;
- sparse and dense weight representation;
- serialization;
- reordered data;
- modified data;
- invalid `loadweights` reuse.

### F040 Exact Kyle Compatibility

Purpose:

- executable Kyle compatibility, not a prose-only migration note.

Checks:

- positional and named calls;
- `first_stage` formulas;
- `wname`;
- `wtr`;
- horizons;
- pretrends;
- clustering;
- multiple outcomes;
- class and column order.

### F041 Python API And Error Surface

Purpose:

- Python-compatible public API and object behavior.

Checks:

- every signature argument;
- defaults;
- placeholder options;
- output fields;
- warnings/messages;
- autosample message;
- invalid combinations.

### F042 Plot Input Validation

Purpose:

- reject malformed manual plotting inputs clearly.

Checks:

- mismatched names;
- partial SEs;
- missing coefficients;
- duplicate terms;
- `NA`/`Inf`;
- alpha limits;
- unsupported manual inputs.

### F043 Plot Multi-Model Semantics

Purpose:

- Stata-like event plot overlay behavior translated into R semantics.

Checks:

- eight models;
- over-eight behavior;
- stub collisions;
- trim boundaries;
- perturbation;
- shifts;
- legend order;
- style recycling.

### F044 Plot Output And Filesystem Behavior

Purpose:

- rendering and save-path behavior under automated tests.

Checks:

- headless rendering;
- file type;
- dimensions;
- overwrite policy;
- invalid paths;
- no device leakage;
- no global theme mutation.

### F045 Published/Example Replication

Purpose:

- reproduce public examples as curated evidence.

Checks:

- Stata `five_estimators_example.do`;
- Python README examples;
- Kyle packaged examples;
- curated expected tables.

### F046 Randomized Differential Testing

Purpose:

- discover edge cases beyond handpicked fixtures.

Checks:

- hundreds of small seeded panels compared against applicable references;
- minimal failing cases retained as regression fixtures.

### F047 Monte Carlo Statistical Sanity

Purpose:

- statistical behavior under known data-generating processes.

Checks:

- known-effect DGP;
- zero-effect DGP;
- bias;
- approximate CI coverage;
- nightly or release-only execution plan.

### F048 Performance Pathology

Purpose:

- stress the performance surfaces most likely to fail.

Checks:

- highly sparse panels;
- many FE levels;
- many horizons;
- many controls;
- `saveweights` densification.

### F049 Object And Method Stability

Purpose:

- stable R object contract.

Checks:

- `print`;
- `summary`;
- `coef`;
- `vcov`;
- `tidy`;
- `glance`;
- `as.data.frame`;
- serialization;
- versioned object upgrades.

### F050 Clean Install And Portability

Purpose:

- package works outside the developer machine.

Checks:

- source tarball install in clean libraries;
- supported OS/R versions;
- no user paths;
- no network access in default tests.

## Evidence Required For Completion

A final implementation report must include:

- feature matrix with every item above marked;
- commands used for R, Python, and Stata parity;
- package check output;
- benchmark table;
- list of known divergences;
- list of blocked claims;
- exact reference commits and package versions.
