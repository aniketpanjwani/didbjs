# Reproducing Parity Evidence

This document describes the public validation model for `didbjs`. It replaces
private machine-specific runbooks with reusable expectations for maintainers.

## Reference Checkouts

Regeneration jobs should use fresh local clones of:

- Borusyak-Jaravel-Spiess Stata `did_imputation`
- the announced Python `did_imputation` package
- Kyle Butts `didimputation`

Store those clones outside the package repository and record the exact commit,
license metadata, and file hashes in fixture manifests.

## Stata Boundary

Stata validation requires a licensed host. Set these environment variables in
your own shell or CI secret store:

```bash
export STATA_BIN=/path/to/stata
export STATA_ADO_ROOT=/path/to/isolated/ado/root
export STATA_REFERENCE_ROOT=/path/to/stata/reference/clone
```

The package does not call Stata during ordinary tests or installation. Stata is
used only to regenerate expected artifacts for parity fixtures.

## Python And R Reference Setup

Use isolated libraries or virtual environments. The regeneration scripts expect
the Python and Kyle references to be installed or available at paths supplied by
environment variables:

```bash
export DIDBJS_PYTHON_REFERENCE=/path/to/python/reference/clone
export DIDBJS_KYLE_REFERENCE=/path/to/kyle/reference/clone
```

## Fixture Classes

- `tests/fixtures/smoke/`: compact fixtures included in the source tarball and
  run by default.
- `tests/fixtures/api/`: compact R-native API fixtures included in the source
  tarball.
- `tests/fixtures/parity/`: heavier full-source parity fixtures. These are
  committed to the repository but excluded from the CRAN-style source tarball.

Full parity tests skip automatically when `tests/fixtures/parity/` is absent.

## Required Regeneration Properties

Each regenerated fixture should record:

- reference repository commits;
- source file hashes;
- generator script hashes;
- runtime versions;
- input fixture hashes;
- output artifact hashes;
- known approved divergences.

Expected outputs must come from independent references or algebraic identities,
not from the `didbjs` implementation under test.

## Public Verification Commands

Default source-package path:

```bash
R CMD build .
R CMD check --as-cran didbjs_0.0.1.9000.tar.gz
```

Full checkout path:

```bash
Rscript -e 'testthat::test_local()'
python3 tools/parity/validate_contract.py
```

Before release, refresh F050 package-check evidence from a fixtures-present
checkout so that the committed manifest records zero skips in the full suite.
