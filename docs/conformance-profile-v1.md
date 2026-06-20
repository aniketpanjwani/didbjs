# Conformance Profile v1

Status: public v1 conformance profile.

This profile defines what `didbjs` v1 treats as complete. A future
implementation may change scope only by creating a new conformance profile or
an accepted decision update.

## Activation Rule

The implementation profile is active because the repository records:

- accepted behavior decisions D001-D015;
- accepted license/provenance posture;
- locked reference commits and hashes;
- numeric benchmark budgets;
- mandatory feature rows in `inst/spec/feature-matrix.csv`;
- first triangular fixture design in
  `docs/triangular-fixture-f001-static-att.md`;
- reference-regeneration requirements in
  `docs/reproducing-parity-evidence.md`.

## Terminal Status Rules

Before implementation starts, mandatory rows in `inst/spec/feature-matrix.csv`
use status `contract-frozen`. This means the scope, normative source, planned
test, artifact path, tolerance policy, decision record, and allowed terminal
statuses are frozen, but implementation evidence has not yet been produced.

Mandatory features may finish only as:

- `parity-verified`
- `approved-divergence`

Mandatory features may not finish as:

- `implemented-only`
- `unsupported-by-design`
- `blocked`

`unsupported-by-design` is allowed only for preapproved exclusions listed in
this profile before implementation begins. `blocked` always means
`terminated-incomplete`, not success.

## Domain-Specific Source Of Truth

| Domain | Governing source | Frozen status |
| --- | --- | --- |
| Estimand definition and statistical equations | BJS Review of Economic Studies article, DOI `10.1093/restud/rdae007`, plus pinned Stata behavior | frozen |
| Stata option semantics and stored diagnostics | Pinned `borusyak/did_imputation` commit `767c8d6670a751170910d419bbafd323df92ef08` and generated Stata outputs | frozen |
| Finite-sample covariance behavior | Pinned Stata outputs plus hand/algebraic oracle fixtures F033-F036 | frozen |
| Python-compatible API and object shape | Pinned Python `did_imputation` commit `c7765a9fb2dcc48dc745b356784b4e9ce8b1d376` | frozen |
| Python-compatible plotting behavior | Pinned Python implementation and generated plot inputs | frozen |
| Kyle compatibility | Pinned Kyle `didimputation` commit `69b4f8dfe16b007474721fc5610859b56a80cdc6` and Kyle public-call fixtures | frozen |
| Primary R API | D002 R-native S3 API plus wrappers | frozen |
| R error classes and method behavior | D002/D015 plus F041/F049/F050 | frozen |
| Language-specific graph syntax | D013 semantic R plotting translation, not literal Stata graph syntax | frozen |

## Compatibility Modes

- Package name: `didbjs`.
- Primary API: R-native estimator returning a stable S3 object.
- Python compatibility: argument aliases, output aliases, and schema extractors
  matching pinned Python behavior where Python supports the feature.
- Stata compatibility: term names, diagnostics, sample masks, covariance, and
  plotting data governed by pinned Stata behavior.
- Kyle compatibility: tested wrapper/migration helpers, including multi-outcome
  behavior.
- Release quality: CRAN-quality package checks are mandatory for v1 even before
  CRAN submission.

## Preapproved Exclusions

| Feature | Scope | Reason | Required user-facing behavior |
| --- | --- | --- | --- |
| Literal Stata `graph_opt` syntax | plotting | R cannot meaningfully parse arbitrary Stata graph syntax | clear error plus R styling alternatives |
| Stata importance/frequency weights | weights | v1 supports analytic weights only | clear unsupported weight-class error |
| Python placeholder arguments not implemented upstream | Python compatibility | placeholders are not supported behavior in the pinned Python package | clear unsupported placeholder error |

These exclusions are nonmandatory rows. Any mandatory feature must finish as
`parity-verified` or `approved-divergence`.

## Mandatory Evidence Surface

- `inst/spec/feature-matrix.csv` is the mandatory feature/fixture matrix.
- `docs/verification-criteria.md` defines fixtures F001-F050.
- `docs/tolerance-registry-v1.md` defines every tolerance identifier used by
  the feature matrix.
- `inst/spec/bench-budgets.yml` defines fail-capable numeric performance gates.
- `tools/parity/reference-lock/` records reference dependency locks used by
  regeneration jobs.
- `docs/triangular-fixture-f001-static-att.md` defines the first complete
  triangular fixture design.
- `docs/reproducing-parity-evidence.md` defines required regeneration
  hardening before new reference outputs can be trusted.

## Triangular Fixture Requirement

The first implementation milestone must complete F001 as specified in
`docs/triangular-fixture-f001-static-att.md` before expanding into dynamic
horizons. F001 must use committed inputs and committed expected artifacts; default
R tests must not call Stata, Python, Kyle, internet, SSH, or user-specific paths.

Bootstrap order for F001:

1. Complete the committed generator stubs under
   `tools/parity/generators/f001-static-att/`.
2. Run the Stata/Python/Kyle regeneration jobs against locked references.
3. Commit expected artifacts and `metadata/manifest.json` under
   `tests/fixtures/smoke/f001-static-att/`.
4. Enable default R tests that consume those committed artifacts and fail if any
   mandatory artifact is absent.

## Completion Gate For Implementation Goal

The implementation goal completes only if:

- every mandatory row in `inst/spec/feature-matrix.csv` has status
  `parity-verified` or `approved-divergence`;
- zero mandatory rows are blocked, implemented-only, or unsupported-by-design;
- all benchmark budgets pass;
- package checks pass on the supported matrix;
- license/provenance records are complete;
- the final report includes artifact hashes and regeneration commands.
