#!/usr/bin/env python3
"""Generate Python reference artifacts for F018 saveweights."""

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

    with contextlib.redirect_stdout(log_buffer), contextlib.redirect_stderr(log_buffer):
        print(f"F018_PYTHON_INPUT={args.input_csv}")
        print(f"F018_PYTHON_OUTPUT={args.output_dir}")
        print(f"PYTHON_VERSION={sys.version}")
        panel = pd.read_csv(args.input_csv)
        output = did_imputation(
            panel,
            y="Y",
            i="unit",
            t="t",
            Ei="Ei",
            fe=[],
            minn=0,
            saveweights=True,
        )
        print("F018_PYTHON_EXPORT_OK=1")

    (args.output_dir / "run.log").write_text(log_buffer.getvalue())
    if output is None:
        raise RuntimeError("did_imputation returned no output")

    panel = pd.read_csv(args.input_csv)
    estimate = float(output.estimates["tau_ate"])
    std_error = float(output.std_errors["tau_ate"])
    covariance = float(scalar(output.V))
    weights = output.weights.copy()
    weight_col = "copywtr"
    weight_rows = []
    for idx, weight in enumerate(weights[weight_col].tolist()):
        weight_rows.append(
            {
                "row_id": panel.loc[idx, "row_id"],
                "term": weight_col,
                "weight": f"{float(weight):.17g}",
            }
        )
    weighted_y_estimate = float(np.sum(weights[weight_col].to_numpy() * panel["Y"].to_numpy()))

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
    write_csv(
        args.output_dir / "covariance.csv",
        [{"row_term": "tau_ate", "col_term": "tau_ate", "value": f"{covariance:.17g}"}],
        ["row_term", "col_term", "value"],
    )
    write_csv(args.output_dir / "weights.csv", weight_rows, ["row_id", "term", "weight"])

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
    (args.output_dir / "object-schema.json").write_text(json.dumps(schema, indent=2) + "\n")

    diagnostics = {
        "status": "success",
        "command": 'did_imputation(panel, y="Y", i="unit", t="t", Ei="Ei", fe=[], minn=0, saveweights=True)',
        "python_version": sys.version,
        "platform": platform.platform(),
        "input_sha256": sha256(args.input_csv),
        "generator_sha256": sha256(pathlib.Path(__file__)),
        "estimate": estimate,
        "std_error": std_error,
        "covariance": covariance,
        "weighted_y_estimate": weighted_y_estimate,
        "warning_text": log_buffer.getvalue(),
    }
    (args.output_dir / "diagnostics.json").write_text(json.dumps(diagnostics, indent=2) + "\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
