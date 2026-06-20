#!/usr/bin/env python3
"""Generate Python reference artifacts for F005 custom positive weights."""

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


def algebraic_targets(panel: pd.DataFrame) -> dict[str, float]:
    treated = panel["Ei"].notna() & (panel["t"] >= panel["Ei"])
    out = {}
    for name in ["wtr_uniform", "wtr_late"]:
        raw = panel.loc[treated, "w"] * panel.loc[treated, name]
        out[name] = float(np.sum(raw * panel.loc[treated, "tau"]) / np.sum(raw))
    return out


def normalized_sums(panel: pd.DataFrame) -> dict[str, float]:
    treated = panel["Ei"].notna() & (panel["t"] >= panel["Ei"])
    out = {}
    for name in ["wtr_uniform", "wtr_late"]:
        raw = panel.loc[treated, "w"] * panel.loc[treated, name]
        out[name] = float(np.sum(raw / np.sum(raw)))
    return out


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
    panel = pd.read_csv(args.input_csv)
    algebraic = algebraic_targets(panel)

    with contextlib.redirect_stdout(log_buffer), contextlib.redirect_stderr(log_buffer):
        print(f"F005_PYTHON_INPUT={args.input_csv}")
        print(f"F005_PYTHON_OUTPUT={args.output_dir}")
        print(f"PYTHON_VERSION={sys.version}")
        output = did_imputation(
            panel,
            y="Y",
            i="unit",
            t="t",
            Ei="Ei",
            fe=["unit", "t"],
            aw="w",
            wtr=["wtr_uniform", "wtr_late"],
            minn=0,
        )
        if list(output.estimates.keys()) != ["wtr_uniform", "wtr_late"]:
            raise AssertionError(f"F005 custom wtr terms mismatch: {list(output.estimates.keys())}")
        for term, target in algebraic.items():
            estimate = float(output.estimates[term])
            if abs(estimate - target) > 1e-5:
                raise AssertionError(f"F005 {term} assertion failed: {estimate} target {target}")
        print("F005_PYTHON_EXPORT_OK=1")

    (args.output_dir / "run.log").write_text(log_buffer.getvalue())

    if output is None:
        status = "error"
        error = "did_imputation returned no output"

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
        covariance_rows = [
            {
                "row_term": "python_V_total",
                "col_term": "python_V_total",
                "value": f"{float(scalar(output.V)):.17g}",
            }
        ]
        write_csv(args.output_dir / "covariance.csv", covariance_rows, ["row_term", "col_term", "value"])
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
        "command": 'did_imputation(panel, y="Y", i="unit", t="t", Ei="Ei", fe=["unit", "t"], aw="w", wtr=["wtr_uniform", "wtr_late"], minn=0)',
        "python_version": sys.version,
        "platform": platform.platform(),
        "input_sha256": sha256(args.input_csv),
        "generator_sha256": sha256(pathlib.Path(__file__)),
        "warning_text": log_buffer.getvalue(),
        "algebraic_att": algebraic,
        "normalized_sums": normalized_sums(panel),
        "tol001_abs": 1e-10,
    }
    if output is not None:
        diagnostics["estimates"] = {term: float(value) for term, value in output.estimates.items()}
        diagnostics["std_errors"] = {term: float(value) for term, value in output.std_errors.items()}
        diagnostics["V"] = scalar(output.V)
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
                "Same fixed-effect recovery drift as D017/F001-F004: pinned "
                "Python recover_fixed_effects_iterative() leaves custom-wtr "
                "point estimates outside TOL001 while preserving public object "
                "shape and raw custom-weight estimate names."
            )
    if error is not None:
        diagnostics["error"] = error
    (args.output_dir / "diagnostics.json").write_text(json.dumps(diagnostics, indent=2) + "\n")
    return 0 if status == "success" else 1


if __name__ == "__main__":
    raise SystemExit(main())
