#!/usr/bin/env python3
"""Generate Python reference artifacts for F007 analytic weights."""

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
    with path.open("w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fields)
        writer.writeheader()
        writer.writerows(rows)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("input_csv", type=pathlib.Path)
    parser.add_argument("output_dir", type=pathlib.Path)
    args = parser.parse_args()

    args.output_dir.mkdir(parents=True, exist_ok=True)
    log_buffer = io.StringIO()
    output = None
    unweighted = None
    status = "success"
    error = None

    with contextlib.redirect_stdout(log_buffer), contextlib.redirect_stderr(log_buffer):
        print(f"F007_PYTHON_INPUT={args.input_csv}")
        print(f"F007_PYTHON_OUTPUT={args.output_dir}")
        print(f"PYTHON_VERSION={sys.version}")
        panel = pd.read_csv(args.input_csv)
        output = did_imputation(
            panel,
            y="Y",
            i="unit",
            t="t",
            Ei="Ei",
            fe=["unit", "t"],
            aw="w",
            minn=0,
        )
        unweighted = did_imputation(
            panel,
            y="Y",
            i="unit",
            t="t",
            Ei="Ei",
            fe=["unit", "t"],
            minn=0,
        )
        if list(output.estimates.keys()) != ["tau_ate"]:
            raise AssertionError(f"F007 analytic-weight terms mismatch: {list(output.estimates.keys())}")
        if abs(float(output.estimates["tau_ate"]) - float(unweighted.estimates["tau_ate"])) < 1e-6:
            raise AssertionError("F007 analytic-weight probe did not change the Python estimate enough")
        print("F007_PYTHON_EXPORT_OK=1")

    (args.output_dir / "run.log").write_text(log_buffer.getvalue())

    if output is None:
        status = "error"
        error = "did_imputation returned no output"

    if output is not None:
        write_csv(
            args.output_dir / "estimates.csv",
            [
                {
                    "term": "tau_ate",
                    "estimate": f"{float(output.estimates['tau_ate']):.17g}",
                    "std_error": f"{float(output.std_errors['tau_ate']):.17g}",
                    "n_obs": int(output.n_obs),
                }
            ],
            ["term", "estimate", "std_error", "n_obs"],
        )
        write_csv(
            args.output_dir / "covariance.csv",
            [{"row_term": "python_V_total", "col_term": "python_V_total", "value": f"{float(scalar(output.V)):.17g}"}],
            ["row_term", "col_term", "value"],
        )
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
        }
        (args.output_dir / "object-schema.json").write_text(json.dumps(schema, indent=2) + "\n")

    panel = pd.read_csv(args.input_csv)
    treated = panel["Ei"].notna() & (panel["t"] >= panel["Ei"])
    raw = panel.loc[treated, "w"]
    diagnostics = {
        "status": status,
        "command": 'did_imputation(panel, y="Y", i="unit", t="t", Ei="Ei", fe=["unit", "t"], aw="w", minn=0)',
        "python_version": sys.version,
        "platform": platform.platform(),
        "input_sha256": sha256(args.input_csv),
        "generator_sha256": sha256(pathlib.Path(__file__)),
        "warning_text": log_buffer.getvalue(),
        "treated_weight_sum": float(np.sum(raw)),
        "normalized_weight_sum": float(np.sum(raw / np.sum(raw))),
        "tol001_abs": 1e-10,
    }
    if output is not None:
        diagnostics["estimate"] = float(output.estimates["tau_ate"])
        diagnostics["std_error"] = float(output.std_errors["tau_ate"])
        diagnostics["V"] = float(scalar(output.V))
    if unweighted is not None and output is not None:
        diagnostics["unweighted_estimate"] = float(unweighted.estimates["tau_ate"])
        diagnostics["weighted_unweighted_abs_diff"] = abs(
            diagnostics["estimate"] - diagnostics["unweighted_estimate"]
        )
    if error is not None:
        diagnostics["error"] = error
    (args.output_dir / "diagnostics.json").write_text(json.dumps(diagnostics, indent=2) + "\n")
    return 0 if status == "success" else 1


if __name__ == "__main__":
    raise SystemExit(main())
