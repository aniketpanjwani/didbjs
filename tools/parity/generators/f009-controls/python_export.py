#!/usr/bin/env python3
"""Generate Python reference artifacts for F009 continuous controls."""

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


CONTROLS = ["x1", "x2"]


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
    collinearity = {"status": "not_run"}

    with contextlib.redirect_stdout(log_buffer), contextlib.redirect_stderr(log_buffer):
        print(f"F009_PYTHON_INPUT={args.input_csv}")
        print(f"F009_PYTHON_OUTPUT={args.output_dir}")
        print(f"PYTHON_VERSION={sys.version}")
        panel = pd.read_csv(args.input_csv)
        output = did_imputation(
            panel,
            y="Y",
            i="unit",
            t="t",
            Ei="Ei",
            controls=CONTROLS,
            aw="w",
            fe=["unit", "t"],
            minn=0,
        )
        try:
            did_imputation(
                panel,
                y="Y",
                i="unit",
                t="t",
                Ei="Ei",
                controls=["x_bad"],
                aw="w",
                fe=["unit", "t"],
                minn=0,
            )
            collinearity = {"status": "unexpected_success"}
        except Exception as exc:  # noqa: BLE001 - reference error text is the artifact.
            collinearity = {
                "status": "error",
                "type": type(exc).__name__,
                "message": str(exc),
            }
        print("F009_PYTHON_EXPORT_OK=1")

    (args.output_dir / "run.log").write_text(log_buffer.getvalue())

    estimate_rows = [
        {
            "term": "tau_ate",
            "estimate": f"{float(output.estimates['tau_ate']):.17g}",
            "std_error": f"{float(output.std_errors['tau_ate']):.17g}",
            "n_obs": int(output.n_obs),
        }
    ]
    control_rows = [
        {
            "term": control,
            "estimate": f"{float(output.controls_estimates[control]):.17g}",
            "std_error": f"{float(output.controls_std_errors[control]):.17g}",
        }
        for control in CONTROLS
    ]
    covariance_rows = []
    v = scalar(output.V)
    if isinstance(v, list):
        arr = np.asarray(v)
        if arr.ndim == 2:
            for row_idx in range(arr.shape[0]):
                for col_idx in range(arr.shape[1]):
                    covariance_rows.append(
                        {
                            "row_index": row_idx + 1,
                            "col_index": col_idx + 1,
                            "value": f"{float(arr[row_idx, col_idx]):.17g}",
                        }
                    )
        else:
            covariance_rows.append({"row_index": 1, "col_index": 1, "value": f"{float(np.asarray(arr).item()):.17g}"})
    else:
        covariance_rows.append({"row_index": 1, "col_index": 1, "value": f"{float(v):.17g}"})

    schema_fields = {
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
    }
    diagnostics = {
        "status": status,
        "command": 'did_imputation(panel, y="Y", i="unit", t="t", Ei="Ei", controls=["x1", "x2"], aw="w", fe=["unit", "t"], minn=0)',
        "python_version": sys.version,
        "platform": platform.platform(),
        "input_sha256": sha256(args.input_csv),
        "generator_sha256": sha256(pathlib.Path(__file__)),
        "warning_text": log_buffer.getvalue(),
        "controls": CONTROLS,
        "estimates": output.estimates,
        "std_errors": output.std_errors,
        "controls_estimates": output.controls_estimates,
        "controls_std_errors": output.controls_std_errors,
        "n_obs": int(output.n_obs),
        "V": scalar(output.V),
    }

    write_csv(args.output_dir / "estimates.csv", estimate_rows, ["term", "estimate", "std_error", "n_obs"])
    write_csv(args.output_dir / "controls.csv", control_rows, ["term", "estimate", "std_error"])
    write_csv(args.output_dir / "covariance.csv", covariance_rows, ["row_index", "col_index", "value"])
    (args.output_dir / "object-schema.json").write_text(
        json.dumps({"class": "DIDImputationOutput", "fields": schema_fields}, indent=2) + "\n"
    )
    (args.output_dir / "diagnostics.json").write_text(json.dumps(diagnostics, indent=2) + "\n")
    (args.output_dir / "collinearity.json").write_text(json.dumps(collinearity, indent=2) + "\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
