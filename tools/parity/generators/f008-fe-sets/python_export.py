#!/usr/bin/env python3
"""Generate Python reference artifacts for F008 alternative FE sets."""

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


SPECS: dict[str, list[str] | None] = {
    "constant_only": None,
    "unit_only": ["unit"],
    "time_only": ["t"],
    "arbitrary": ["group", "t"],
}


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
    outputs = {}
    status = "success"

    with contextlib.redirect_stdout(log_buffer), contextlib.redirect_stderr(log_buffer):
        print(f"F008_PYTHON_INPUT={args.input_csv}")
        print(f"F008_PYTHON_OUTPUT={args.output_dir}")
        print(f"PYTHON_VERSION={sys.version}")
        panel = pd.read_csv(args.input_csv)
        for spec, fe in SPECS.items():
            outputs[spec] = did_imputation(
                panel,
                y="Y",
                i="unit",
                t="t",
                Ei="Ei",
                fe=fe,
                minn=0,
            )
        print("F008_PYTHON_EXPORT_OK=1")

    (args.output_dir / "run.log").write_text(log_buffer.getvalue())

    estimate_rows = []
    covariance_rows = []
    schema_fields = None
    diagnostics = {
        "status": status,
        "command": 'for each spec, did_imputation(panel, y="Y", i="unit", t="t", Ei="Ei", fe=<spec>, minn=0)',
        "python_version": sys.version,
        "platform": platform.platform(),
        "input_sha256": sha256(args.input_csv),
        "generator_sha256": sha256(pathlib.Path(__file__)),
        "warning_text": log_buffer.getvalue(),
        "specs": list(SPECS.keys()),
        "fe": SPECS,
        "estimates": {},
        "std_errors": {},
        "V": {},
        "n_obs": {},
    }
    for spec, output in outputs.items():
        estimate = float(output.estimates["tau_ate"])
        std_error = float(output.std_errors["tau_ate"])
        estimate_rows.append(
            {
                "spec": spec,
                "term": "tau_ate",
                "estimate": f"{estimate:.17g}",
                "std_error": f"{std_error:.17g}",
                "n_obs": int(output.n_obs),
            }
        )
        covariance_rows.append(
            {
                "spec": spec,
                "row_term": "python_V_total",
                "col_term": "python_V_total",
                "value": f"{float(scalar(output.V)):.17g}",
            }
        )
        diagnostics["estimates"][spec] = estimate
        diagnostics["std_errors"][spec] = std_error
        diagnostics["V"][spec] = float(scalar(output.V))
        diagnostics["n_obs"][spec] = int(output.n_obs)
        if schema_fields is None:
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

    write_csv(args.output_dir / "estimates.csv", estimate_rows, ["spec", "term", "estimate", "std_error", "n_obs"])
    write_csv(args.output_dir / "covariance.csv", covariance_rows, ["spec", "row_term", "col_term", "value"])
    (args.output_dir / "object-schema.json").write_text(
        json.dumps({"class": "DIDImputationOutput", "fields": schema_fields}, indent=2) + "\n"
    )
    (args.output_dir / "diagnostics.json").write_text(json.dumps(diagnostics, indent=2) + "\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
