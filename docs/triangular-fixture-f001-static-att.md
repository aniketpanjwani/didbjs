# Triangular Fixture F001: Static ATT

Status: frozen for conformance profile v1.

Purpose: define the first complete Stata/Python/Kyle/R parity fixture. This
fixture proves the golden-output workflow before estimator implementation
begins and is the required first implementation milestone.

## Scope

Feature coverage:

- static ATT;
- default unit/time fixed effects;
- clustered standard errors by unit;
- full covariance matrix;
- sample mask;
- `N`, `Nc`, `Nt`, and treated-observation counts;
- R object/schema extraction;
- Kyle static ATT compatibility output;
- Python `DIDImputationOutput` schema output.

Out of scope for this fixture:

- dynamic horizons as tested estimates;
- pretrends;
- custom weights;
- autosample failures;
- plotting overlays.

## Input Dataset Contract

Location: `tests/fixtures/parity/f001-static-att/inputs/panel.csv`.

Rows: 60 observations, 10 units, and 6 periods.

Columns:

- `row_id`: stable row identity, formatted as `i_t`;
- `i`: unit id;
- `t`: calendar period;
- `Ei`: first treatment period, blank for never-treated;
- `D`: treatment indicator;
- `event_time`: `t - Ei`, blank for never-treated;
- `Y0`: untreated potential outcome;
- `tau`: treatment effect applied to treated observations, zero otherwise;
- `Y`: observed outcome;
- `w`: analytic weight, always one.

Data-generating process:

- units 1-5 are treated in period 4;
- units 6-10 are never treated;
- `Y0 = 10 * i + t`;
- for treated observations, `tau = 1 + event_time + (i - 3) / 10`;
- for untreated observations, `tau = 0`;
- `Y = Y0 + tau`.

Hand/algebraic oracle:

- no row has missing outcome, id, time, treatment indicator, or weight;
- 45 observations are untreated or not-yet-treated for first-stage fitting;
- 15 observations are treated post-treatment observations;
- untreated potential outcomes are exactly additive in unit and time fixed
  effects, so imputation residuals on treated observations equal `tau`;
- horizon-specific average effects are `tau0 = 1`, `tau1 = 2`, and `tau2 = 3`;
- the static ATT is `tau = 2`;
- all expected point estimates must be exact to `TOL001`.

The unit-level treatment-effect perturbation is deliberate. It keeps the point
estimate hand-solvable while avoiding a zero-variance fixture for clustered
standard errors.

## Reference Exporters

Stata exporter:

- path: `tools/parity/generators/f001-static-att/stata-export.do`;
- command shape:
  `did_imputation Y i t Ei [aw=w], minn(0) cluster(i)`;
- must run in a licensed Stata environment;
- must record pinned Stata repo commit and ado hashes in fixture metadata;
- must export `e(b)`, `e(V)`, `e(sample)`, `e(Nc)`, `e(Nt)`, and raw logs;
- must export numeric values with 17 significant digits and preserve matrix
  stripes;
- must assert static ATT `2` before writing expected artifacts.

Python exporter:

- path: `tools/parity/generators/f001-static-att/python_export.py`;
- command shape:
  `did_imputation(df, y="Y", i="i", t="t", Ei="Ei", fe=["i", "t"], aw="w", minn=0)`;
- must use pinned Python commit and locked dependencies;
- must export estimates, standard errors, covariance if available, object
  fields, schema, and missing-value conventions;
- must assert static ATT `2` before writing expected artifacts.

Kyle exporter:

- path: `tools/parity/generators/f001-static-att/kyle-export.R`;
- command shape:
  `did_imputation(data = panel, yname = "Y", gname = "Ei", tname = "t", idname = "i", wname = "w", cluster_var = "i")`;
- must use pinned Kyle commit and locked R dependencies;
- must export exact public-call output, column names, row order, confidence
  intervals, classes, and warnings/messages;
- must assert static ATT `2` before writing expected artifacts.

R test contract:

- consumes committed expected artifacts only;
- does not regenerate goldens during default tests;
- fails if any mandatory artifact is absent;
- compares all applicable references, not a selective oracle;
- records an approved divergence only when the divergence is predeclared in the
  conformance profile or a decision record.

## Tolerance Contract

Authoritative definitions live in `docs/tolerance-registry-v1.md`. This fixture
uses the following subset:

Tolerance IDs:

- `TOL001`: point estimates, absolute tolerance `1e-10`;
- `TOL002`: standard errors and covariance entries, absolute tolerance `1e-8`;
- `TOL003`: Kyle public-output schema and confidence interval numerics,
  absolute tolerance `1e-8`;
- `TOL006`: sample mask, row ids, term names, matrix stripes, column names, row
  order, and classes must match exactly.

Tolerance changes after implementation require a behavior decision record and
old-versus-new artifact diff.

## Bootstrap Sequence

This contract-freeze branch commits the deterministic input and generator
contract stubs. The first implementation milestone must complete the generator
scripts, produce and commit expected Stata/Python/Kyle artifacts plus
`metadata/manifest.json`, and only then enable default tests that fail on absent
goldens. Until that bootstrap step is complete, the absence of expected outputs
is an implementation precondition, not a successful default-test state.

## Artifact Layout

```text
tests/fixtures/parity/f001-static-att/
  inputs/
    panel.csv
  expected/
    stata/
      estimates.csv
      covariance.csv
      sample-mask.csv
      diagnostics.json
      run.log
    python/
      estimates.csv
      covariance.csv
      object-schema.json
      diagnostics.json
      run.log
    kyle/
      estimates.csv
      output-schema.json
      diagnostics.json
      run.log
  metadata/
    manifest.json
    f036-algebraic-oracle.json
    f036-horizon-effects.csv
    f036-treated-weights.csv
    f036-manifest.json
tools/parity/generators/f001-static-att/
  stata-export.do
  python_export.py
  kyle-export.R
tools/parity/generators/f036-algebraic-oracle/
  algebraic-export.py
```

Generated expected artifacts must include the reference commit, dependency-lock
hash, input-file hash, generator-file hash, generation command, timestamp, host,
and success status. Default package tests must fail if those metadata fields are
missing.
