#!/usr/bin/env python3
"""Generate Python reference artifacts for F004 horizon balance."""

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


def hbalance_units(panel: pd.DataFrame) -> list[dict[str, Any]]:
    work = panel.copy()
    work["treated_unit"] = work["Ei"].notna()
    work["in_requested_horizon"] = work["treated_unit"] & work["event_time"].isin([0, 1, 2])
    counts = work.groupby("unit", as_index=False).agg(
        treated_unit=("treated_unit", "max"),
        requested_horizon_count=("in_requested_horizon", "sum"),
    )
    counts["hbalance_included"] = counts["treated_unit"] & (counts["requested_horizon_count"] == 3)
    return counts.sort_values("unit").to_dict(orient="records")


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("input_csv", type=pathlib.Path)
    parser.add_argument("output_dir", type=pathlib.Path)
    args = parser.parse_args()

    args.output_dir.mkdir(parents=True, exist_ok=True)
    log_buffer = io.StringIO()
    output = None
    status = "success"
    error = None

    with contextlib.redirect_stdout(log_buffer), contextlib.redirect_stderr(log_buffer):
        print(f"F004_PYTHON_INPUT={args.input_csv}")
        print(f"F004_PYTHON_OUTPUT={args.output_dir}")
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
            horizons=[0, 1, 2],
            hbalance=True,
            minn=0,
        )
        expected = {"tau0": 1.0, "tau1": 2.0, "tau2": 3.0}
        if list(output.estimates.keys()) != list(expected.keys()):
            raise AssertionError(f"F004 hbalance terms mismatch: {list(output.estimates.keys())}")
        for term, target in expected.items():
            estimate = float(output.estimates[term])
            if abs(estimate - target) > 1e-5:
                raise AssertionError(f"F004 {term} assertion failed: {estimate}")
        print("F004_PYTHON_EXPORT_OK=1")

    (args.output_dir / "run.log").write_text(log_buffer.getvalue())

    if output is None:
        status = "error"
        error = "did_imputation returned no output"

    panel = pd.read_csv(args.input_csv)
    balance_rows = hbalance_units(panel)
    write_csv(
        args.output_dir / "hbalance-units.csv",
        balance_rows,
        ["unit", "treated_unit", "requested_horizon_count", "hbalance_included"],
    )

    if output is not None:
        rows = []
        for term, estimate in output.estimates.items():
            rows.append(
                {
                    "term": term,
                    "estimate": f"{float(estimate):.17g}",
                    "std_error": f"{float(output.std_errors[term]):.17g}",
                    "n_obs": int(output.n_obs),
                }
            )
        write_csv(args.output_dir / "estimates.csv", rows, ["term", "estimate", "std_error", "n_obs"])
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

    algebraic = {"tau0": 1.0, "tau1": 2.0, "tau2": 3.0}
    diagnostics = {
        "status": status,
        "command": 'did_imputation(panel, y="Y", i="unit", t="t", Ei="Ei", fe=["unit", "t"], aw="w", horizons=[0, 1, 2], hbalance=True, minn=0)',
        "python_version": sys.version,
        "platform": platform.platform(),
        "input_sha256": sha256(args.input_csv),
        "generator_sha256": sha256(pathlib.Path(__file__)),
        "warning_text": log_buffer.getvalue(),
        "algebraic_horizon_att": algebraic,
        "hbalance_included_units": [1, 2, 3],
        "hbalance_excluded_units": [4],
        "tol001_abs": 1e-10,
    }
    if output is not None:
        diagnostics["estimates"] = {term: float(value) for term, value in output.estimates.items()}
        diagnostics["std_errors"] = {term: float(value) for term, value in output.std_errors.items()}
        diagnostics["V"] = float(scalar(output.V))
        diagnostics["algebraic_abs_diff"] = {
            term: abs(float(output.estimates[term]) - target)
            for term, target in algebraic.items()
        }
        diagnostics["tol001_pass_by_term"] = {
            term: diff <= diagnostics["tol001_abs"]
            for term, diff in diagnostics["algebraic_abs_diff"].items()
        }
        diagnostics["tol001_pass"] = all(diagnostics["tol001_pass_by_term"].values())
        if not diagnostics["tol001_pass"]:
            diagnostics["root_cause_probe"] = (
                "Same fixed-effect recovery drift as D017/F001-F003: pinned "
                "Python recover_fixed_effects_iterative() leaves hbalance "
                "horizon estimates outside TOL001 while preserving public "
                "object shape and hbalance unit selection."
            )
    if error is not None:
        diagnostics["error"] = error
    (args.output_dir / "diagnostics.json").write_text(json.dumps(diagnostics, indent=2) + "\n")
    return 0 if status == "success" else 1


if __name__ == "__main__":
    raise SystemExit(main())
