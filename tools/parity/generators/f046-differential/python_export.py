#!/usr/bin/env python3
"""Generate Python reference artifacts for F046 randomized differential tests."""

from __future__ import annotations

import argparse
import contextlib
import csv
import hashlib
import io
import json
import pathlib
import platform
import sys
from typing import Any

import numpy as np
import pandas as pd
from did_imputation import did_imputation


def scalar(value: Any) -> Any:
    if isinstance(value, np.generic):
        return value.item()
    if isinstance(value, np.ndarray):
        return value.tolist()
    return value


def sha256(path: pathlib.Path) -> str:
    h = hashlib.sha256()
    with path.open("rb") as f:
        for chunk in iter(lambda: f.read(1024 * 1024), b""):
            h.update(chunk)
    return h.hexdigest()


def write_csv(path: pathlib.Path, rows: list[dict[str, Any]], fields: list[str]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fields, lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)


def term_rows(output: Any, scenario: str, estimand: str) -> list[dict[str, Any]]:
    rows = []
    for term, estimate in output.estimates.items():
        rows.append(
            {
                "scenario": scenario,
                "estimand": estimand,
                "term": term,
                "estimate": f"{float(estimate):.17g}",
                "std_error": f"{float(output.std_errors[term]):.17g}",
                "n_obs": int(output.n_obs),
            }
        )
    return rows


def covariance_rows(output: Any, scenario: str) -> list[dict[str, Any]]:
    terms = list(output.estimates.keys())
    matrix = np.asarray(output.V)
    if matrix.ndim == 0:
        matrix = matrix.reshape((1, 1))
    if matrix.shape != (len(terms), len(terms)):
        return [
            {
                "scenario": scenario,
                "row_term": "python_V_total",
                "col_term": "python_V_total",
                "value": f"{float(np.sum(matrix)):.17g}",
            }
        ]
    rows = []
    for r, row_term in enumerate(terms):
        for c, col_term in enumerate(terms):
            rows.append(
                {
                    "scenario": scenario,
                    "row_term": row_term,
                    "col_term": col_term,
                    "value": f"{float(matrix[r, c]):.17g}",
                }
            )
    return rows


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("input_dir", type=pathlib.Path)
    parser.add_argument("output_dir", type=pathlib.Path)
    args = parser.parse_args()
    args.output_dir.mkdir(parents=True, exist_ok=True)

    scenarios = pd.read_csv(args.input_dir / "scenarios.csv")
    panel = pd.read_csv(args.input_dir / "panels.csv")
    estimate_rows: list[dict[str, Any]] = []
    covariance: list[dict[str, Any]] = []
    failures: list[dict[str, Any]] = []
    log_buffer = io.StringIO()

    with contextlib.redirect_stdout(log_buffer), contextlib.redirect_stderr(log_buffer):
        print(f"F046_PYTHON_INPUT_DIR={args.input_dir}")
        print(f"F046_PYTHON_OUTPUT={args.output_dir}")
        print(f"PYTHON_VERSION={sys.version}")
        for spec in scenarios.itertuples(index=False):
            scenario = str(spec.scenario)
            estimand = str(spec.estimand)
            scenario_panel = panel.loc[panel["scenario"] == scenario].copy()
            kwargs = {
                "df": scenario_panel,
                "y": "Y",
                "i": "unit",
                "t": "t",
                "Ei": "Ei",
                "fe": ["unit", "t"],
                "minn": 0,
            }
            if int(spec.weighted) == 1:
                kwargs["aw"] = "w"
            if estimand == "dynamic":
                kwargs["horizons"] = [0, 1, 2]
            try:
                output = did_imputation(**kwargs)
            except Exception as exc:  # pragma: no cover - reference-generation guard
                failures.append(
                    {
                        "scenario": scenario,
                        "reference": "python",
                        "failure_class": type(exc).__name__,
                        "failure_message": str(exc),
                        "retained_fixture_path": "tests/fixtures/parity/f046-differential/inputs/panels.csv",
                    }
                )
                print(f"F046_PYTHON_FAILURE {scenario} {type(exc).__name__}: {exc}")
                continue
            estimate_rows.extend(term_rows(output, scenario, estimand))
            covariance.extend(covariance_rows(output, scenario))
        print("F046_PYTHON_EXPORT_OK=1")

    write_csv(
        args.output_dir / "estimates.csv",
        estimate_rows,
        ["scenario", "estimand", "term", "estimate", "std_error", "n_obs"],
    )
    write_csv(
        args.output_dir / "covariance.csv",
        covariance,
        ["scenario", "row_term", "col_term", "value"],
    )
    write_csv(
        args.output_dir / "failures.csv",
        failures,
        ["scenario", "reference", "failure_class", "failure_message", "retained_fixture_path"],
    )
    (args.output_dir / "run.log").write_text(log_buffer.getvalue())

    diagnostics = {
        "status": "success" if not failures else "reference_failures",
        "command": 'did_imputation(panel, y="Y", i="unit", t="t", Ei="Ei", fe=["unit", "t"], aw=<scenario>, horizons=<scenario>, minn=0)',
        "python_version": sys.version,
        "platform": platform.platform(),
        "scenario_count": int(len(scenarios)),
        "static_count": int((scenarios["estimand"] == "static").sum()),
        "dynamic_count": int((scenarios["estimand"] == "dynamic").sum()),
        "weighted_count": int((scenarios["weighted"] == 1).sum()),
        "estimate_rows": len(estimate_rows),
        "failure_count": len(failures),
        "input_sha256": {
            "panels": sha256(args.input_dir / "panels.csv"),
            "scenarios": sha256(args.input_dir / "scenarios.csv"),
        },
        "generator_sha256": sha256(pathlib.Path(__file__)),
        "warning_text": log_buffer.getvalue(),
        "approved_numeric_drift": "D017 applies when comparing pinned Python estimates to Stata/R-native core estimates.",
    }
    (args.output_dir / "diagnostics.json").write_text(json.dumps(diagnostics, indent=2, sort_keys=True) + "\n")
    return 0 if not failures else 1


if __name__ == "__main__":
    raise SystemExit(main())
