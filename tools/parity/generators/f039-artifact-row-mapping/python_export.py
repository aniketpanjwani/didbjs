#!/usr/bin/env python3
"""Generate Python reference artifacts for F039 saved-artifact row mapping."""

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


SCENARIOS = ("base", "reordered")


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
    with path.open("w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fields)
        writer.writeheader()
        writer.writerows(rows)


def run_reference(input_dir: pathlib.Path, scenario: str):
    panel = pd.read_csv(input_dir / f"{scenario}.csv")
    return panel, did_imputation(
        panel,
        y="Y",
        i="unit",
        t="t",
        Ei="Ei",
        fe=["unit", "t"],
        aw="w",
        horizons=[0, 1, 2],
        minn=0,
        saveweights=True,
    )


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("input_dir", type=pathlib.Path)
    parser.add_argument("output_dir", type=pathlib.Path)
    args = parser.parse_args()
    args.output_dir.mkdir(parents=True, exist_ok=True)

    log_buffer = io.StringIO()
    outputs = {}
    with contextlib.redirect_stdout(log_buffer), contextlib.redirect_stderr(log_buffer):
        print(f"F039_PYTHON_INPUT_DIR={args.input_dir}")
        print(f"F039_PYTHON_OUTPUT={args.output_dir}")
        print(f"PYTHON_VERSION={sys.version}")
        for scenario in SCENARIOS:
            outputs[scenario] = run_reference(args.input_dir, scenario)
        print("F039_PYTHON_EXPORT_OK=1")

    (args.output_dir / "run.log").write_text(log_buffer.getvalue())

    estimate_rows = []
    covariance_rows = []
    dense_rows = []
    sparse_rows = []
    schema = None
    for scenario, (panel, output) in outputs.items():
        for term, estimate in output.estimates.items():
            estimate_rows.append(
                {
                    "scenario": scenario,
                    "term": term,
                    "estimate": f"{float(estimate):.17g}",
                    "std_error": f"{float(output.std_errors[term]):.17g}",
                    "n_obs": int(output.n_obs),
                }
            )
        covariance_rows.append(
            {
                "scenario": scenario,
                "row_term": "python_V_total",
                "col_term": "python_V_total",
                "value": f"{float(scalar(output.V)):.17g}",
            }
        )
        weights = output.weights.copy()
        if schema is None:
            schema = {
                "class": type(output).__name__,
                "fields": {
                    name: {
                        "present": hasattr(output, name),
                        "type": type(getattr(output, name, None)).__name__,
                        "is_null": getattr(output, name, None) is None,
                    }
                    for name in [
                        "pretrends_estimates",
                        "pretrends_std_errors",
                        "estimates",
                        "std_errors",
                        "controls_estimates",
                        "controls_std_errors",
                        "n_obs",
                        "weights",
                        "V",
                    ]
                },
                "weights_columns": list(weights.columns),
            }
        for col in weights.columns:
            term = col.replace("copywtr", "tau")
            if term == "tau":
                term = "tau_ate"
            for idx, weight in enumerate(weights[col].tolist()):
                row = {
                    "scenario": scenario,
                    "row_id": panel.loc[idx, "row_id"],
                    "term": term,
                    "weight": f"{float(weight):.17g}",
                }
                dense_rows.append(row)
                if abs(float(weight)) > 1e-12:
                    sparse_rows.append(row)

    write_csv(args.output_dir / "estimates.csv", estimate_rows, ["scenario", "term", "estimate", "std_error", "n_obs"])
    write_csv(args.output_dir / "covariance.csv", covariance_rows, ["scenario", "row_term", "col_term", "value"])
    write_csv(args.output_dir / "weights-dense.csv", dense_rows, ["scenario", "row_id", "term", "weight"])
    write_csv(args.output_dir / "weights-sparse.csv", sparse_rows, ["scenario", "row_id", "term", "weight"])
    (args.output_dir / "object-schema.json").write_text(json.dumps(schema, indent=2) + "\n")

    diagnostics = {
        "status": "success",
        "command": 'did_imputation(panel, y="Y", i="unit", t="t", Ei="Ei", fe=["unit", "t"], aw="w", horizons=[0, 1, 2], minn=0, saveweights=True)',
        "python_version": sys.version,
        "platform": platform.platform(),
        "input_hashes": {scenario: sha256(args.input_dir / f"{scenario}.csv") for scenario in SCENARIOS},
        "generator_sha256": sha256(pathlib.Path(__file__)),
        "dense_row_count": len(dense_rows),
        "dense_nonzero_count": sum(abs(float(row["weight"])) > 1e-12 for row in dense_rows),
        "sparse_row_count": len(sparse_rows),
        "warning_text": log_buffer.getvalue(),
    }
    (args.output_dir / "diagnostics.json").write_text(json.dumps(diagnostics, indent=2) + "\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
