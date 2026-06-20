#!/usr/bin/env python3
"""Generate Python reference artifacts for F001 static ATT."""

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
    status = "success"
    error = None
    output = None

    with contextlib.redirect_stdout(log_buffer), contextlib.redirect_stderr(log_buffer):
        print(f"F001_PYTHON_INPUT={args.input_csv}")
        print(f"F001_PYTHON_OUTPUT={args.output_dir}")
        print(f"PYTHON_VERSION={sys.version}")
        panel = pd.read_csv(args.input_csv)
        output = did_imputation(
            panel,
            y="Y",
            i="i",
            t="t",
            Ei="Ei",
            fe=["i", "t"],
            aw="w",
            minn=0,
        )
        estimate = float(output.estimates["tau_ate"])
        if abs(estimate - 2.0) > 1e-5:
            raise AssertionError(f"F001 static ATT assertion failed: {estimate}")
        print("F001_PYTHON_EXPORT_OK=1")

    (args.output_dir / "run.log").write_text(log_buffer.getvalue())

    if output is None:
        status = "error"
        error = "did_imputation returned no output"

    if output is not None:
        estimate = float(output.estimates["tau_ate"])
        std_error = float(output.std_errors["tau_ate"])
        write_csv(
            args.output_dir / "estimates.csv",
            [
                {
                    "term": "tau_ate",
                    "estimate": f"{estimate:.17g}",
                    "std_error": f"{std_error:.17g}",
                    "n_obs": int(output.n_obs),
                }
            ],
            ["term", "estimate", "std_error", "n_obs"],
        )
        v = scalar(output.V)
        write_csv(
            args.output_dir / "covariance.csv",
            [{"row_term": "tau_ate", "col_term": "tau_ate", "value": f"{float(v):.17g}"}],
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

    diagnostics = {
        "status": status,
        "command": 'did_imputation(panel, y="Y", i="i", t="t", Ei="Ei", fe=["i", "t"], aw="w", minn=0)',
        "python_version": sys.version,
        "platform": platform.platform(),
        "input_sha256": sha256(args.input_csv),
        "generator_sha256": sha256(pathlib.Path(__file__)),
        "warning_text": log_buffer.getvalue(),
        "algebraic_static_att": 2.0,
        "tol001_abs": 1e-10,
    }
    if output is not None:
        diagnostics["estimate"] = estimate
        diagnostics["std_error"] = std_error
        diagnostics["covariance"] = float(scalar(output.V))
        diagnostics["algebraic_abs_diff"] = abs(estimate - diagnostics["algebraic_static_att"])
        diagnostics["tol001_pass"] = diagnostics["algebraic_abs_diff"] <= diagnostics["tol001_abs"]
        if not diagnostics["tol001_pass"]:
            diagnostics["root_cause_probe"] = (
                "Pinned Python reference uses recover_fixed_effects_iterative() "
                "with max_iter=100 and tol=1e-6; local tightening probes reduce "
                "but do not eliminate F001 point-estimate drift under TOL001."
            )
    if error is not None:
        diagnostics["error"] = error
    (args.output_dir / "diagnostics.json").write_text(json.dumps(diagnostics, indent=2) + "\n")
    return 0 if status == "success" else 1


if __name__ == "__main__":
    raise SystemExit(main())
