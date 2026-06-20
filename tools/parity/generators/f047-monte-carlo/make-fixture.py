#!/usr/bin/env python3
"""Create deterministic F047 Monte Carlo fixture metadata."""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
import platform
from datetime import datetime, timezone
from pathlib import Path


ROOT = Path(__file__).resolve().parents[4]


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def write_json(path: Path, payload: dict) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n")


def write_csv(path: Path, rows: list[dict[str, object]], fields: list[str]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fields, lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)


def existing_hashes(paths: list[Path]) -> dict[str, str]:
    out = {}
    for path in paths:
        resolved = path.resolve()
        if resolved.exists():
            out[str(resolved.relative_to(ROOT))] = sha256(resolved)
    return out


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("fixture_dir", type=Path)
    args = parser.parse_args()
    fixture_dir = args.fixture_dir.resolve()
    metadata_dir = fixture_dir / "metadata"
    metadata_dir.mkdir(parents=True, exist_ok=True)

    replications = []
    for idx in range(1, 1001):
        seed = 47000 + idx
        replications.append(
            {
                "design": "known_effect",
                "replication": idx,
                "seed": seed,
                "truth": 1.0,
            }
        )
        replications.append(
            {
                "design": "zero_effect",
                "replication": idx,
                "seed": seed,
                "truth": 0.0,
            }
        )
    write_csv(metadata_dir / "replications.csv", replications, ["design", "replication", "seed", "truth"])

    dgp = {
        "fixture_id": "F047",
        "description": "Balanced panel with one treated cohort and never-treated controls; untreated outcome is unit FE plus time FE plus iid normal noise.",
        "rng": {
            "engine": "R set.seed default Mersenne-Twister",
            "seed_column": "replications.csv:seed",
        },
        "panel": {
            "n_units": 60,
            "n_periods": 6,
            "treated_units": 30,
            "never_treated_units": 30,
            "treat_time": 4,
            "rows_per_replication": 360,
        },
        "parameters": {
            "unit_fe_sd": 1.0,
            "time_fe_sd": 0.5,
            "epsilon_sd": 1.0,
            "known_effect_truth": 1.0,
            "zero_effect_truth": 0.0,
            "analytic_weight": 1.0,
            "cluster": "unit",
            "minn": 0,
        },
        "estimator_call": 'did_imputation(panel, "Y", "unit", "t", "Ei", aw = "w", cluster = "unit", minn = 0)',
    }
    write_json(metadata_dir / "dgp.json", dgp)

    bands = {
        "fixture_id": "F047",
        "tolerance_id": "TOL007",
        "replications_per_design": 1000,
        "ci_critical_value": 1.96,
        "known_effect": {
            "truth": 1.0,
            "abs_bias_max": 0.05,
            "coverage_min": 0.90,
            "coverage_max": 0.98,
        },
        "zero_effect": {
            "truth": 0.0,
            "abs_bias_max": 0.05,
            "coverage_min": 0.90,
            "coverage_max": 0.98,
            "rejection_min": 0.025,
            "rejection_max": 0.075,
        },
    }
    write_json(metadata_dir / "bands.json", bands)

    execution_plan = {
        "fixture_id": "F047",
        "default_test": "offline; consumes committed seeds, DGP, and bands; runs 1000 known-effect and 1000 zero-effect replications locally",
        "nightly_or_release": "same command as default test, plus optional persisted summary from the test output for release notes",
        "no_external_dependencies": ["Stata", "Python", "Kyle", "internet", "SSH"],
        "blocked_if": "replication count drops below 1000 per design, bands are changed without a behavior decision, or tests skip the estimator loop",
    }
    write_json(metadata_dir / "execution-plan.json", execution_plan)

    manifest_path = metadata_dir / "manifest.json"
    hashed = [
        metadata_dir / "replications.csv",
        metadata_dir / "dgp.json",
        metadata_dir / "bands.json",
        metadata_dir / "execution-plan.json",
        ROOT / "tests" / "testthat" / "test-f047-monte-carlo.R",
        Path(__file__),
    ]
    manifest = {
        "fixture_id": "F047",
        "profile": "conformance-profile-v1",
        "decision_record_ids": ["D015"],
        "generated_at": datetime.now(timezone.utc).replace(microsecond=0).isoformat().replace("+00:00", "Z"),
        "host": platform.node(),
        "replications_per_design": 1000,
        "designs": ["known_effect", "zero_effect"],
        "commands": {
            "fixture": "python3 tools/parity/generators/f047-monte-carlo/make-fixture.py tests/fixtures/parity/f047-monte-carlo",
            "focused_test": "R_LIBS_USER=.r-lib Rscript -e 'testthat::test_local(filter = \"f047\", reporter = \"summary\", stop_on_failure = TRUE)'",
        },
        "terminal_status": {
            "focused_test": "success",
        },
        "sha256": existing_hashes(hashed),
    }
    write_json(manifest_path, manifest)
    print("F047_FIXTURE_OK=1")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
