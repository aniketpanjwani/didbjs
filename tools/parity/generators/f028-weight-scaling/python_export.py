#!/usr/bin/env python3
"""Generate Python reference artifacts for F028 analytic-weight scaling."""

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


def run_reference(panel: pd.DataFrame, aw: str = "w"):
    return did_imputation(
        panel,
        y="Y",
        i="unit",
        t="t",
        Ei="Ei",
        fe=["unit", "t"],
        aw=aw,
        minn=0,
    )


def probe(panel: pd.DataFrame, mutate) -> dict[str, Any]:
    candidate = panel.copy()
    mutate(candidate)
    try:
        output = run_reference(candidate)
        return {
            "status": "reference_success",
            "estimate": float(output.estimates["tau_ate"]),
            "std_error": float(output.std_errors["tau_ate"]),
            "n_obs": int(output.n_obs),
        }
    except Exception as exc:  # noqa: BLE001 - reference behavior artifact.
        return {
            "status": "reference_error",
            "type": type(exc).__name__,
            "message": str(exc),
        }


def set_weight(df: pd.DataFrame, row_id: str, value: float) -> None:
    df.loc[df["row_id"] == row_id, "w"] = value


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("input_csv", type=pathlib.Path)
    parser.add_argument("output_dir", type=pathlib.Path)
    args = parser.parse_args()

    args.output_dir.mkdir(parents=True, exist_ok=True)
    log_buffer = io.StringIO()

    with contextlib.redirect_stdout(log_buffer), contextlib.redirect_stderr(log_buffer):
        print(f"F028_PYTHON_INPUT={args.input_csv}")
        print(f"F028_PYTHON_OUTPUT={args.output_dir}")
        print(f"PYTHON_VERSION={sys.version}")
        panel = pd.read_csv(args.input_csv)
        base = run_reference(panel, "w")
        scaled = run_reference(panel, "w_scaled")
        probes = {
            "missing_weight": probe(panel, lambda df: set_weight(df, "2_3", np.nan)),
            "zero_weight": probe(panel, lambda df: set_weight(df, "1_3", 0.0)),
            "negative_weight": probe(panel, lambda df: set_weight(df, "1_3", -1.0)),
            "infinite_weight": probe(panel, lambda df: set_weight(df, "1_3", np.inf)),
            "all_zero_weight": probe(panel, lambda df: df.__setitem__("w", 0.0)),
        }
        print("F028_PYTHON_EXPORT_OK=1")

    (args.output_dir / "run.log").write_text(log_buffer.getvalue())

    estimates = []
    covariance = []
    for scenario, output in [("base", base), ("scaled", scaled)]:
        estimates.append(
            {
                "scenario": scenario,
                "term": "tau_ate",
                "estimate": f"{float(output.estimates['tau_ate']):.17g}",
                "std_error": f"{float(output.std_errors['tau_ate']):.17g}",
                "n_obs": int(output.n_obs),
            }
        )
        covariance.append(
            {
                "scenario": scenario,
                "row_term": "python_V_total",
                "col_term": "python_V_total",
                "value": f"{float(scalar(output.V)):.17g}",
            }
        )

    schema_fields = {
        name: {
            "present": hasattr(base, name),
            "type": type(getattr(base, name, None)).__name__,
            "is_null": getattr(base, name, None) is None,
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
        "base_command": 'did_imputation(panel, y="Y", i="unit", t="t", Ei="Ei", fe=["unit", "t"], aw="w", minn=0)',
        "scaled_command": 'did_imputation(panel, y="Y", i="unit", t="t", Ei="Ei", fe=["unit", "t"], aw="w_scaled", minn=0)',
        "python_version": sys.version,
        "platform": platform.platform(),
        "input_sha256": sha256(args.input_csv),
        "generator_sha256": sha256(pathlib.Path(__file__)),
        "warning_text": log_buffer.getvalue(),
        "base_estimate": float(base.estimates["tau_ate"]),
        "scaled_estimate": float(scaled.estimates["tau_ate"]),
        "base_std_error": float(base.std_errors["tau_ate"]),
        "scaled_std_error": float(scaled.std_errors["tau_ate"]),
        "base_v": float(scalar(base.V)),
        "scaled_v": float(scalar(scaled.V)),
        "estimate_scale_abs_diff": abs(float(base.estimates["tau_ate"]) - float(scaled.estimates["tau_ate"])),
        "variance_scale_abs_diff": abs(float(scalar(base.V)) - float(scalar(scaled.V))),
    }

    write_csv(args.output_dir / "estimates.csv", estimates, ["scenario", "term", "estimate", "std_error", "n_obs"])
    write_csv(args.output_dir / "covariance.csv", covariance, ["scenario", "row_term", "col_term", "value"])
    (args.output_dir / "object-schema.json").write_text(
        json.dumps({"class": "DIDImputationOutput", "fields": schema_fields}, indent=2) + "\n"
    )
    (args.output_dir / "diagnostics.json").write_text(json.dumps(diagnostics, indent=2) + "\n")
    (args.output_dir / "invalid-probes.json").write_text(json.dumps(probes, indent=2) + "\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
