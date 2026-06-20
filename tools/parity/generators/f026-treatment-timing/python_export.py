#!/usr/bin/env python3
"""Generate Python reference artifacts for F026 treatment-timing encodings."""

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

    with contextlib.redirect_stdout(log_buffer), contextlib.redirect_stderr(log_buffer):
        print(f"F026_PYTHON_INPUT={args.input_csv}")
        print(f"F026_PYTHON_OUTPUT={args.output_dir}")
        print(f"PYTHON_VERSION={sys.version}")
        panel = pd.read_csv(args.input_csv)
        output = did_imputation(
            panel,
            y="Y",
            i="unit",
            t="t",
            Ei="Ei",
            aw="w",
            fe=None,
            minn=0,
            cluster="unit",
        )
        print("F026_PYTHON_EXPORT_OK=1")

    (args.output_dir / "run.log").write_text(log_buffer.getvalue())

    estimate_rows = [
        {
            "term": "tau_ate",
            "estimate": f"{float(output.estimates['tau_ate']):.17g}",
            "std_error": f"{float(output.std_errors['tau_ate']):.17g}",
            "n_obs": int(output.n_obs),
        }
    ]
    covariance_rows = [
        {"row_term": "python_V_total", "col_term": "python_V_total", "value": f"{float(scalar(output.V)):.17g}"}
    ]
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
        "status": "success",
        "command": 'did_imputation(panel, y="Y", i="unit", t="t", Ei="Ei", aw="w", fe=None, minn=0, cluster="unit")',
        "python_version": sys.version,
        "platform": platform.platform(),
        "input_sha256": sha256(args.input_csv),
        "generator_sha256": sha256(pathlib.Path(__file__)),
        "warning_text": log_buffer.getvalue(),
        "rows_input": int(len(panel)),
        "estimates": output.estimates,
        "std_errors": output.std_errors,
        "n_obs": int(output.n_obs),
        "V": scalar(output.V),
    }

    write_csv(args.output_dir / "estimates.csv", estimate_rows, ["term", "estimate", "std_error", "n_obs"])
    write_csv(args.output_dir / "covariance.csv", covariance_rows, ["row_term", "col_term", "value"])
    (args.output_dir / "object-schema.json").write_text(
        json.dumps({"class": "DIDImputationOutput", "fields": schema_fields}, indent=2) + "\n"
    )
    (args.output_dir / "diagnostics.json").write_text(json.dumps(diagnostics, indent=2) + "\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
