# Tolerance Registry v1

Status: frozen for conformance profile v1.

This registry is the single source of truth for tolerance identifiers in
`inst/spec/feature-matrix.csv`. Tests and regeneration jobs must fail if a
mandatory row references an identifier absent from this file.

| ID | Applies to | Frozen rule |
| --- | --- | --- |
| `TOL001` | point estimates and algebraic scalar estimands | absolute tolerance `1e-10` |
| `TOL002` | standard errors and covariance matrix entries | absolute tolerance `1e-8` |
| `TOL003` | Kyle public-output numeric columns and confidence intervals | absolute tolerance `1e-8`; public columns, row order, and classes exact |
| `TOL004` | `event_plot` numeric plot data, CI coordinates, shifts, perturbation offsets, and trim endpoints | absolute tolerance `1e-8`; labels, group names, term order, and file-save behavior exact |
| `TOL005` | benchmark budget rows | pass/fail exact against every numeric `max_*` threshold in `bench/budgets.yml`; missing baseline or missing budget is failure |
| `TOL006` | sample masks, row ids, term names, matrix stripes, column names, row order, option defaults, error classes, warnings, and object classes | exact match |
| `TOL007` | Monte Carlo statistical sanity fixtures | effect estimate mean within `0.05` of truth, empirical coverage in `[0.90, 0.98]`, rejection rate under null in `[0.025, 0.075]`, using at least `1000` seeded replications |
| `TOL008` | package build, install, check, and portability gates | exact pass/fail gate: zero errors, zero warnings, no mandatory skips, no network/Stata/Python/SSH/default-test dependency |
| `EXACT` | schema-only and API-contract rows | alias for `TOL006` exact-match semantics |

Tolerance changes require a behavior decision record, old-versus-new artifact
diff, and evidence that the discrepancy is numerical rather than semantic.
