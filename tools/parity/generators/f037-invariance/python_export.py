#!/usr/bin/env python3
"""Generate Python reference artifacts for F037 transformation invariance."""

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


SCENARIOS = (
    "base",
    "row_permuted",
    "unit_relabel",
    "time_shift",
    "outcome_scaled",
    "constant_shift",
    "weight_scaled",
)


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
    return did_imputation(
        panel,
        y="Y",
        i="unit",
        t="t",
        Ei="Ei",
        fe=["unit", "t"],
        aw="w",
        minn=0,
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
        print(f"F037_PYTHON_INPUT_DIR={args.input_dir}")
        print(f"F037_PYTHON_OUTPUT={args.output_dir}")
        print(f"PYTHON_VERSION={sys.version}")
        for scenario in SCENARIOS:
            outputs[scenario] = run_reference(args.input_dir, scenario)
        print("F037_PYTHON_EXPORT_OK=1")

    (args.output_dir / "run.log").write_text(log_buffer.getvalue())
    estimate_rows = []
    covariance_rows = []
    for scenario, output in outputs.items():
        estimate = float(output.estimates["tau_ate"])
        std_error = float(output.std_errors["tau_ate"])
        variance = float(scalar(output.V))
        estimate_rows.append(
            {
                "scenario": scenario,
                "term": "tau_ate",
                "estimate": f"{estimate:.17g}",
                "std_error": f"{std_error:.17g}",
                "variance": f"{variance:.17g}",
                "n_obs": int(output.n_obs),
            }
        )
        covariance_rows.append(
            {
                "scenario": scenario,
                "row_term": "tau_ate",
                "col_term": "tau_ate",
                "value": f"{variance:.17g}",
            }
        )

    write_csv(args.output_dir / "estimates.csv", estimate_rows, ["scenario", "term", "estimate", "std_error", "variance", "n_obs"])
    write_csv(args.output_dir / "covariance.csv", covariance_rows, ["scenario", "row_term", "col_term", "value"])
    base = outputs["base"]
    base_estimate = float(base.estimates["tau_ate"])
    base_se = float(base.std_errors["tau_ate"])
    base_variance = float(scalar(base.V))
    diagnostics = {
        "status": "success",
        "command": 'did_imputation(panel, y="Y", i="unit", t="t", Ei="Ei", fe=["unit", "t"], aw="w", minn=0)',
        "python_version": sys.version,
        "platform": platform.platform(),
        "input_hashes": {scenario: sha256(args.input_dir / f"{scenario}.csv") for scenario in SCENARIOS},
        "generator_sha256": sha256(pathlib.Path(__file__)),
        "warning_text": log_buffer.getvalue(),
        "base_estimate": base_estimate,
        "row_permutation_abs_diff": abs(float(outputs["row_permuted"].estimates["tau_ate"]) - base_estimate),
        "unit_relabel_abs_diff": abs(float(outputs["unit_relabel"].estimates["tau_ate"]) - base_estimate),
        "time_shift_abs_diff": abs(float(outputs["time_shift"].estimates["tau_ate"]) - base_estimate),
        "constant_shift_abs_diff": abs(float(outputs["constant_shift"].estimates["tau_ate"]) - base_estimate),
        "weight_scale_abs_diff": abs(float(outputs["weight_scaled"].estimates["tau_ate"]) - base_estimate),
        "outcome_scale": 3.5,
        "outcome_scaled_estimate_ratio": float(outputs["outcome_scaled"].estimates["tau_ate"]) / base_estimate,
        "outcome_scaled_se_ratio": float(outputs["outcome_scaled"].std_errors["tau_ate"]) / base_se,
        "outcome_scaled_variance_ratio": float(scalar(outputs["outcome_scaled"].V)) / base_variance,
    }
    (args.output_dir / "diagnostics.json").write_text(json.dumps(diagnostics, indent=2) + "\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
