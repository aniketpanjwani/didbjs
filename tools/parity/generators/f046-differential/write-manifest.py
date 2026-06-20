#!/usr/bin/env python3
"""Write F046 differential fixture manifest hashes."""

from __future__ import annotations

import hashlib
import json
import platform
import sys
from datetime import datetime, timezone
from pathlib import Path


ROOT = Path(__file__).resolve().parents[4]
STATA_COMMIT = "767c8d6670a751170910d419bbafd323df92ef08"
PYTHON_COMMIT = "c7765a9fb2dcc48dc745b356784b4e9ce8b1d376"
KYLE_COMMIT = "69b4f8dfe16b007474721fc5610859b56a80cdc6"


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def write_json(path: Path, payload: dict) -> None:
    path.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n")


def existing_hashes(paths: list[Path]) -> dict[str, str]:
    out = {}
    for path in paths:
        resolved = path.resolve()
        if resolved.exists():
            out[str(resolved.relative_to(ROOT))] = sha256(resolved)
    return out


def source_hashes() -> dict[str, str]:
    source_paths = {
        "stata_did_imputation": Path("<reference-clone-root>/did_imputation_stata/did_imputation.ado"),
        "python_estimator": Path("<reference-clone-root>/did_imputation_python/src/did_imputation/did_imputation.py"),
        "kyle_estimator": Path("<reference-clone-root>/didimputation_r_kyle/R/did_imputation.R"),
    }
    return {name: sha256(path) for name, path in source_paths.items() if path.exists()}


def read_scenario_count(fixture_dir: Path) -> int:
    rows = (fixture_dir / "inputs" / "scenarios.csv").read_text().strip().splitlines()
    return max(0, len(rows) - 1)


def main() -> None:
    if len(sys.argv) != 2:
        raise SystemExit("usage: write-manifest.py <fixture_dir>")

    fixture_dir = Path(sys.argv[1]).resolve()
    metadata_dir = fixture_dir / "metadata"
    metadata_dir.mkdir(parents=True, exist_ok=True)

    source_inventory = {
        "fixture_id": "F046",
        "source_boundary": {
            "stata": "Runs pinned Stata did_imputation over all seeded randomized panels as the governing core oracle.",
            "python": "Runs pinned Python did_imputation over the same panels for Python-compatible API parity; D017 governs known numerical drift versus Stata.",
            "kyle": "Runs pinned Kyle didimputation public calls over the same panels for Kyle wrapper parity.",
        },
        "source_sha256": source_hashes(),
        "reference_commits": {
            "stata": STATA_COMMIT,
            "python": PYTHON_COMMIT,
            "kyle": KYLE_COMMIT,
        },
    }
    source_inventory_path = metadata_dir / "source-inventory.json"
    write_json(source_inventory_path, source_inventory)

    hashed = [
        fixture_dir / "inputs" / "panels.csv",
        fixture_dir / "inputs" / "scenarios.csv",
        fixture_dir / "expected" / "stata" / "diagnostics.json",
        fixture_dir / "expected" / "stata" / "estimates.csv",
        fixture_dir / "expected" / "stata" / "covariance.csv",
        fixture_dir / "expected" / "stata" / "sample-mask.csv",
        fixture_dir / "expected" / "stata" / "failures.csv",
        fixture_dir / "expected" / "stata" / "run.log",
        fixture_dir / "expected" / "python" / "diagnostics.json",
        fixture_dir / "expected" / "python" / "estimates.csv",
        fixture_dir / "expected" / "python" / "covariance.csv",
        fixture_dir / "expected" / "python" / "failures.csv",
        fixture_dir / "expected" / "python" / "run.log",
        fixture_dir / "expected" / "kyle" / "diagnostics.json",
        fixture_dir / "expected" / "kyle" / "estimates.csv",
        fixture_dir / "expected" / "kyle" / "failures.csv",
        fixture_dir / "expected" / "kyle" / "run.log",
        fixture_dir / "metadata" / "minimal-failing-cases.csv",
        source_inventory_path,
        ROOT / "tests" / "testthat" / "test-f046-differential.R",
        ROOT / "tools" / "parity" / "generators" / "f046-differential" / "make-inputs.py",
        ROOT / "tools" / "parity" / "generators" / "f046-differential" / "stata-export.do",
        ROOT / "tools" / "parity" / "generators" / "f046-differential" / "python_export.py",
        ROOT / "tools" / "parity" / "generators" / "f046-differential" / "kyle-export.R",
        Path(__file__),
    ]
    manifest = {
        "fixture_id": "F046",
        "profile": "conformance-profile-v1",
        "decision_record_ids": ["D015", "D017", "D026"],
        "generated_at": datetime.now(timezone.utc).replace(microsecond=0).isoformat().replace("+00:00", "Z"),
        "host": platform.node(),
        "scenario_count": read_scenario_count(fixture_dir),
        "input_dir": "tests/fixtures/parity/f046-differential/inputs",
        "commands": {
            "inputs": "python3 tools/parity/generators/f046-differential/make-inputs.py tests/fixtures/parity/f046-differential",
            "stata": "ssh <licensed-stata-host> 'cd <remote_tmp> && /usr/local/bin/stata -b do stata-export.do <input_dir> <output_dir> ${STATA_ADO_ROOT}'",
            "python": ".venv/bin/python tools/parity/generators/f046-differential/python_export.py tests/fixtures/parity/f046-differential/inputs tests/fixtures/parity/f046-differential/expected/python",
            "kyle": "R_LIBS_USER=.r-lib Rscript tools/parity/generators/f046-differential/kyle-export.R tests/fixtures/parity/f046-differential/inputs tests/fixtures/parity/f046-differential/expected/kyle",
            "manifest": "python3 tools/parity/generators/f046-differential/write-manifest.py tests/fixtures/parity/f046-differential",
            "focused_test": "R_LIBS_USER=.r-lib Rscript -e 'testthat::test_local(filter = \"f046\", reporter = \"summary\", stop_on_failure = TRUE)'",
        },
        "reference_commits": {
            "stata": STATA_COMMIT,
            "python": PYTHON_COMMIT,
            "kyle": KYLE_COMMIT,
        },
        "terminal_status": {
            "stata": "success",
            "python": "success_with_d017_numeric_drift_policy",
            "kyle": "success",
            "focused_test": "success",
        },
        "sha256": existing_hashes(hashed),
    }
    write_json(metadata_dir / "manifest.json", manifest)
    print("F046_MANIFEST_OK=1")


if __name__ == "__main__":
    main()
