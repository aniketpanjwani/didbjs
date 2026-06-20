# didbjs

`didbjs` is an R implementation of the Borusyak-Jaravel-Spiess
difference-in-differences imputation estimator with event-study plotting,
Stata/Python-informed behavior, and a Kyle-style R compatibility wrapper.

The package is built around a public verification contract:

- Stata is treated as the black-box behavioral reference for estimator,
  inference, diagnostics, and Stata-like plotting semantics.
- The announced Python package is treated as the reference for Python-style
  object/API behavior where it overlaps.
- Kyle Butts's R package is treated as the reference for existing R ergonomics
  and migration tests.
- Algebraic smoke fixtures are included in the source package so the default
  check path does not require Stata, Python, SSH, or internet access.
- The full parity fixture corpus remains in the repository and is excluded from
  the CRAN-style source tarball.

## Installation

```r
install.packages("pak")
pak::pak(".")
```

or, from a built source package:

```r
install.packages("didbjs_0.0.1.9000.tar.gz", repos = NULL, type = "source")
```

## Basic Use

```r
library(didbjs)

fit <- did_imputation(
  data = panel,
  y = "Y",
  i = "unit",
  t = "year",
  Ei = "first_treated_year",
  horizons = -3:5,
  cluster = "unit"
)

summary(fit)
event_plot(fit)
```

For Kyle-style calls, use `did_imputation_kyle()`. For Python-style output
shape, use `did_imputation_python()`.

## Validation

The ordinary package path is intentionally offline:

```bash
R CMD build .
R CMD check --as-cran didbjs_0.0.1.9000.tar.gz
```

The source checkout includes heavyweight parity fixtures under
`tests/fixtures/parity/`. They are ignored from the source tarball but run in a
full checkout:

```r
testthat::test_local()
```

Useful public verification references:

- `docs/conformance-profile-v1.md`
- `docs/behavior-decisions.md`
- `docs/tolerance-registry-v1.md`
- `docs/verification-criteria.md`
- `docs/parity-verification-playbook.md`
- `docs/reproducing-parity-evidence.md`
- `PROVENANCE.md`

## License And Provenance

The package code is released under MIT. Stata reference code is GPL-3.0 and is
not copied or translated into this package; Stata is used only as a licensed
black-box reference to generate expected behavior. MIT-licensed Python and Kyle
R references are used for compatibility, with adapted or derived material
recorded in `PROVENANCE.md`.
