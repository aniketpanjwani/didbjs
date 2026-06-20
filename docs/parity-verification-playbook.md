# Parity Verification Playbook

This playbook is the instruction set for a future implementation goal. It is
deliberately stricter than a normal package checklist: the package is done only
when the evidence shows that the R implementation is complete, ergonomic,
documented, performant, and honest about every divergence from the Python,
Stata, and Kyle R references.

## North Star

Build an R package that an applied researcher can trust for BJS
did-imputation workflows without first reading three reference repos.

The package should:

- reproduce canonical Stata estimator behavior where Stata is the source of
  truth;
- reproduce the announced Python package's public API and object behavior where
  Python is the target interface;
- give Kyle Butts's existing R users a compatibility path or explicit migration
  path;
- include `event_plot`, not just the estimator;
- expose enough diagnostics that users can understand sample drops, imputation
  failures, suppressed estimates, pretrend tests, and covariance behavior;
- ship with tests and generated fixtures that make every parity claim
  independently auditable.

## Non-Negotiable Verification Rule

Every public behavior must end in one of these states:

- `parity-verified`: covered by an automated comparison to Stata, Python, Kyle
  R, or an algebraic invariant.
- `implemented-only`: covered by R unit tests but no external reference exists.
- `approved-divergence`: intentionally differs from a reference, with a
  pre-existing decision record, a reason, a user-facing note, and a regression
  test for the chosen behavior.
- `unsupported-by-design`: rejected with a clear error before computation.
- `blocked`: not claimable; the blocker and exact unlock condition are recorded.

No option, output field, warning, saved artifact, plot coordinate, or benchmark
claim should remain unclassified.

Mandatory feature rows are fail-closed:

- successful terminal statuses are only `parity-verified` or
  `approved-divergence`;
- `implemented-only` is allowed only for nonmandatory R-only features;
- `unsupported-by-design` is allowed only for exclusions preapproved in
  `docs/conformance-profile-v1.md` before implementation begins;
- `blocked` always means `terminated-incomplete`, never successful completion.

## Evidence Files To Produce

The future implementation should generate these repo-local artifacts:

- `tests/fixtures/parity/{fixture}/metadata/manifest.json`: reference commits,
  package versions, Stata ado versions, OS/runtime details, input hashes,
  generator hashes, and fixture generation timestamp.
- `inst/spec/feature-matrix.csv`: one row per option/output/behavior with
  implementation status, reference source, test file, and evidence artifact.
- `tests/fixtures/parity/{fixture}/inputs/*.csv`: deterministic input panels
  shared by R, Python, Kyle, and Stata.
- `tests/fixtures/parity/{fixture}/expected/stata/*`: exported Stata
  coefficients, covariance matrices, diagnostics, and raw logs.
- `tests/fixtures/parity/{fixture}/expected/python/*`: exported Python outputs
  and plot data.
- `tests/fixtures/parity/{fixture}/expected/kyle/*`: Kyle package outputs on
  compatibility fixtures.
- `reports/parity-summary.md`: human-readable completion report.
- `tools/parity/generators/{fixture}/*`: committed regeneration scripts whose
  hashes are recorded in each fixture manifest.
- `bench/results/*.csv`: benchmark results with row counts, memory, timings, and
  comparison package versions.

Run `python3 tools/parity/validate_contract.py` before changing any matrix or
tolerance registry row.

Generated reference outputs may be committed if they are compact and stable.
Large or machine-specific benchmark outputs should be regenerated and recorded
in the final report instead of treated as source fixtures.

## Source-Of-Truth Hierarchy

Use this hierarchy unless a specific fixture documents an exception:

1. Stata `borusyak/did_imputation` is canonical for estimator semantics,
   option meaning, stored diagnostics, and `event_plot` Stata compatibility.
2. Python `gmarinichev/did_imputation` is canonical for the announced Python API,
   `DIDImputationOutput` shape, and Python-compatible plotting behavior.
3. Kyle Butts `didimputation` is canonical for existing R ergonomics,
   backwards-compatible function arguments, and migration tests.
4. Algebraic identities are canonical where no reference exposes the
   intermediate quantity.

When references disagree:

- create the smallest fixture that isolates the disagreement;
- export Stata, Python, and Kyle outputs where possible;
- document the chosen behavior in `feature_matrix.csv`;
- add a test that asserts the chosen behavior and names the discarded behavior.

## Tolerance Policy

Use `docs/tolerance-registry-v1.md` as the single authoritative tolerance
source. The feature matrix must reference only IDs defined there, including
`EXACT` as the alias for exact-match semantics.

A tolerance exception or change must record the fixture, compared references,
observed drift, old-versus-new artifact diff, suspected cause, and why the
looser threshold is still scientifically harmless.

Do not hide systematic disagreement behind broad tolerance. If every large
fixture differs in the same direction, treat it as a bug or documented
divergence.

## Test Taxonomy

Use four layers of tests.

Unit tests:

- argument normalization;
- treatment timing and relative time;
- FE/control parsing;
- weight normalization;
- invalid option combinations;
- output object construction;
- print, summary, `coef`, `vcov`, `tidy`, and `glance` methods.

Fixture parity tests:

- deterministic input data;
- expected external outputs;
- estimates, SEs, covariance matrices, diagnostics, saved weights, saved
  residuals, and plot data;
- skipped only when the relevant external runtime is unavailable and the skip
  message names the required setup command.

Property tests:

- saved weights reproduce estimates as weighted sums of outcomes;
- zero-effect fixtures produce zero estimates;
- shifting all outcomes by a constant preserves ATT under proper FE;
- duplicated never-treated controls with normalized weights do not change
  estimates when analytically expected;
- adding pretrend tests does not change post-treatment effects;
- `nose=TRUE` removes SE computation without changing point estimates.

Package/release tests:

- `R CMD check`;
- `R CMD check --as-cran` if CRAN release is a target;
- vignettes build;
- README examples run;
- no tests require internet access by default;
- integration tests are opt-in through environment variables.

## Fixture Suite Requirements

The fixture suite in `docs/verification-criteria.md` is the minimum. The
implementation goal should expand each fixture into:

- one data generator or static data file;
- one R test;
- one Python exporter where Python supports the feature;
- one Stata `.do` exporter where Stata supports the feature;
- expected output files;
- a short fixture README entry with purpose, reference source, and known
  divergences.

Each fixture must be deterministic and small enough for CI unless marked as a
benchmark fixture.

### Core Estimator Fixtures

Verify:

- static ATT with default unit/time FE;
- dynamic `horizons`;
- `allhorizons`;
- `hbalance`;
- custom positive `wtr`;
- negative `wtr` under `sum=TRUE`;
- analytic weights;
- no FE, unit-only FE, period-only FE, arbitrary FE list;
- continuous controls;
- unit-interacted and time-interacted controls when included in the frozen
  conformance profile;
- repeated cross-section semantics;
- triple-difference style composite IDs/interacted FE;
- `shift`;
- non-unit `delta`;
- failed imputation and autosample behavior;
- `minn` suppression;
- multiple outcomes when Kyle multi-outcome compatibility is included in the
  frozen conformance profile (see D014).

### Standard-Error Fixtures

Verify:

- default cluster by unit;
- alternate cluster variable;
- `nose`;
- default `avgeffectsby = Ei, t`;
- custom `avgeffectsby`;
- controls in `V`;
- pretrends in `V`;
- leave-out SEs when included in the frozen conformance profile;
- convergence behavior under `tol` and `maxit` when included in the frozen
  conformance profile;
- small-cohort and low-effective-count warnings.

### Saved-Artifact Fixtures

Verify:

- `saveweights` exports enough information to reproduce estimates;
- `loadweights` reproduces estimates for a second outcome when included in the
  frozen conformance profile;
- `saveestimates` creates observation-level imputed treatment effects;
- `saveresid` creates residuals used for SE computation;
- `N`, `Nc`, `Nt`, dropped rows, trimmed rows, suppressed terms, and iteration
  count are preserved in stable output fields.

### Pretrend Fixtures

Verify:

- no pretrends by default;
- `pretrends=k` creates `pre1...prek`;
- pretrends use untreated observations only;
- post-treatment estimates are unchanged by adding pretrends;
- reference group matches Stata's all periods before `-k` plus never-treated
  convention;
- individual pretrend SEs;
- joint F statistic, p value, and degrees of freedom where implemented.

### Plot Fixtures

Verify:

- result-object input;
- manual estimate/SE input;
- `rcap`;
- `rarea`;
- pretrend/effect sorting;
- confidence interval coordinates;
- `together=TRUE` and `together=FALSE`;
- no-SE behavior;
- zero line, event line, labels, legend, theme controls, and save path;
- Stata stubs, lead/lag trimming, multiple model overlays, perturbation,
  per-model style options, `noplot`, and `savecoef` where implemented.

Prefer tests on extracted plot data and `ggplot_build()` output. Use visual
snapshot tests only after the plot data is already covered.

## API Standards

A complete package should expose two friendly layers:

- a primary R API that feels natural in R and returns a stable S3 object;
- compatibility helpers for Python/Stata/Kyle naming where useful.

Minimum API expectations:

- one estimator function with clear argument names and defaults;
- one `event_plot()` function;
- stable object class;
- stable term naming rules;
- `print()` and `summary()` that are concise but include sample diagnostics;
- `coef()`, `vcov()`, `tidy()`, and `glance()` methods;
- `as.data.frame()` or equivalent tidy extraction;
- plot-data extraction for tests and advanced users;
- explicit migration helper or wrapper for Kyle's `did_imputation()` signature.

Do not optimize for cleverness at the expense of inspectability. Researchers
must be able to ask: which observations were used, which were dropped, what was
estimated, and why a coefficient is missing.

## User-Facing Quality Bar

The package should feel polished, not merely ported.

Documentation:

- README has one static ATT example, one event-study example, one pretrend
  example, one plotting example, and one migration note for Kyle users.
- Reference docs explain every option, default, incompatibility, and saved
  diagnostic field.
- Vignettes cover estimator intuition, Stata/Python parity, plotting, and
  troubleshooting.
- Error messages say what happened and what to change.

Examples:

- runnable without external Stata or Python;
- use packaged example data;
- avoid hidden internet access;
- show how to inspect dropped rows and saved weights.

Package hygiene:

- no global option mutations;
- no uncontrolled temp files;
- deterministic seeds in examples/tests;
- no noisy startup messages;
- CRAN-friendly imports and namespace;
- clear license posture.

## Performance Standards

Kirill's thread specifically flags Kyle's package as basic with limited
functionality and computational efficiency. The port should therefore benchmark
both correctness and scale.

Benchmark dimensions:

- rows: small, medium, large;
- number of units;
- number of periods;
- number of treated cohorts;
- number of horizons/pretrends;
- number and type of fixed effects;
- number of continuous controls;
- analytic weights;
- saved weights and covariance construction.

Required comparisons:

- new R package versus Kyle on overlapping features;
- new R package versus Python on all overlapping benchmark features in the
  frozen conformance profile;
- memory allocation for the estimator core and plotting separately.

Minimum claim standard:

- small fixtures should not be materially slower than Kyle for overlapping
  static/horizon cases;
- medium fixtures should complete predictably without excessive allocation;
- large fixtures can be opt-in, but their results must be reported before any
  public claim about efficiency.

## Stata Parity Workflow

Use a maintainer-controlled licensed Stata setup and provide the relevant paths
through environment variables:

```bash
export STATA_BIN=/path/to/stata
export STATA_ADO_ROOT=/path/to/isolated/ado/root
export STATA_REFERENCE_ROOT=/path/to/stata/reference/clone
```

The exporter should run with base `stata`, an isolated ado path under
`${STATA_ADO_ROOT}`, and machine-readable outputs copied back to this repo.

Stata logs are audit evidence, not test assertions. Tests should consume CSV,
JSON, Matrix Market, or RDS artifacts with explicit row and column names.

## Python Parity Workflow

Use an isolated Python environment and a pinned reference clone:

```bash
export DIDBJS_PYTHON_REFERENCE=/path/to/python/reference/clone
python -m pip install -e "$DIDBJS_PYTHON_REFERENCE" pandas numpy pyhdfe statsmodels scipy matplotlib
```

Python exporters should emit:

- estimates;
- standard errors;
- covariance matrices when available;
- output object field names;
- optional weights;
- plot data derived from the Python plotting inputs.

If Python lacks a Stata feature, do not treat missing Python output as a block.
Mark the fixture as Stata-only or algebraic-invariant-only.

## Kyle Compatibility Workflow

Kyle's package should be used for:

- smoke tests proving the existing examples still have a migration path;
- overlapping static ATT, horizon, pretrend, weighted, and multiple-outcome
  cases;
- performance baseline on overlapping features;
- output ergonomics comparison.

The new package need not mimic every Kyle internal decision. It must either
support Kyle-style calls or provide a clear, tested migration wrapper.

## Licensing Boundary

Before coding estimator internals, decide and record package licensing.

Default safe posture:

- MIT Python and Kyle R code may be studied and reused subject to their license
  terms.
- GPL-3 Stata code should be treated as behavior reference only unless the new
  package is deliberately GPL-compatible.
- Generated numerical outputs and behavioral specs may be used as parity
  targets.

The final report must state whether any implementation code was copied or
adapted from references.

## Integration-Test Controls

Default `testthat` should run without Python, Stata, internet, SSH, licensed Stata host, or
user-specific paths by consuming committed expected artifacts.

Use environment variables for heavier checks:

- `DID_R_RUN_PYTHON_PARITY=1`
- `DID_R_RUN_STATA_PARITY=1`
- `DID_R_RUN_KYLE_COMPAT=1`
- `DID_R_RUN_BENCHMARKS=1`

Mandatory tests must not skip in the release gate. Regeneration jobs may depend
on external runtimes and should point at setup commands when unavailable.

## Completion Gates

Gate 1: Spec completeness.

- every Stata option and Python argument mapped;
- every output field mapped;
- every Kyle public argument mapped;
- every fixture has an owner test or is explicitly out of scope.

Gate 2: Estimator correctness.

- core fixtures pass for point estimates;
- standard-error fixtures pass or document divergence;
- saved artifact and pretrend fixtures pass.

Gate 3: Plot correctness.

- Python-compatible plot data tests pass;
- Stata-compatible plot feature decisions are implemented or documented;
- visual snapshots are stable if used.

Gate 4: Ergonomics and docs.

- README, help pages, and vignettes cover the main workflows;
- errors and warnings are tested;
- Kyle migration path is tested.

Gate 5: Performance.

- benchmark suite runs;
- Kyle/Python comparisons are reported;
- any known bottleneck is documented.

Gate 6: Release hygiene.

- all default tests pass;
- integration parity tests pass in the prepared environments;
- package checks pass;
- final report records reference commits and known divergences.

## Failure Triage Loop

Between implementation iterations:

1. pick the highest-value failing fixture, unmapped option, benchmark bottleneck,
   package-check failure, or unclear source-of-truth decision;
2. inspect the reference code/docs and generated outputs for that single issue;
3. patch the smallest relevant surface;
4. rerun the narrow test;
5. rerun the relevant gate;
6. update the feature matrix and final report.

Stop only when all gates pass or the same blocker prevents progress after the
unlock condition has been made explicit.

## Final Report Template

The implementation goal's final report should include:

- reference commits and runtime versions;
- feature matrix summary by status;
- fixture pass/fail table;
- tolerance exceptions;
- approved divergences;
- unsupported-by-design features;
- blocked items and unlock conditions;
- package-check output summary;
- benchmark summary;
- license/source-use statement;
- commands needed to regenerate parity artifacts.
