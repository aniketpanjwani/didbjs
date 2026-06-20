#!/usr/bin/env python3
"""Generate Python reference artifacts for F030 horizon input boundaries."""

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


SCENARIOS: list[tuple[str, dict[str, Any], str]] = [
    ("unsorted", {"horizons": [2, 0]}, 'horizons=[2, 0]'),
    ("sparse", {"horizons": [0, 2]}, 'horizons=[0, 2]'),
    ("absent", {"horizons": [0, 3]}, 'horizons=[0, 3]'),
    ("duplicate", {"horizons": [0, 0]}, 'horizons=[0, 0]'),
    ("negative", {"horizons": [-1]}, 'horizons=[-1]'),
    ("empty", {"horizons": []}, 'horizons=[]'),
    (
        "horizons_allhorizons",
        {"horizons": [0, 1], "allhorizons": True},
        'horizons=[0, 1], allhorizons=True',
    ),
]


def scalar(value: Any) -> Any:
    if isinstance(value, np.generic):
        return value.item()
    if isinstance(value, np.ndarray):
        return value.tolist()
    return value


def jsonable(value: Any) -> Any:
    value = scalar(value)
    if isinstance(value, float) and not np.isfinite(value):
        return str(value)
    if isinstance(value, dict):
        return {str(k): jsonable(v) for k, v in value.items()}
    if isinstance(value, (list, tuple)):
        return [jsonable(v) for v in value]
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


def run_scenario(panel: pd.DataFrame, kwargs: dict[str, Any]) -> tuple[str, Any, str | None, str]:
    log_buffer = io.StringIO()
    output = None
    error = None
    status = "reference_success"
    with contextlib.redirect_stdout(log_buffer), contextlib.redirect_stderr(log_buffer):
        try:
            output = did_imputation(
                panel.copy(),
                y="Y",
                i="unit",
                t="t",
                Ei="Ei",
                fe=["unit", "t"],
                aw="w",
                minn=0,
                **kwargs,
            )
        except Exception as exc:  # noqa: BLE001 - reference probe records public behavior.
            status = "reference_error"
            error = f"{type(exc).__name__}: {exc}"
    return status, output, error, log_buffer.getvalue()


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("input_csv", type=pathlib.Path)
    parser.add_argument("output_dir", type=pathlib.Path)
    args = parser.parse_args()

    args.output_dir.mkdir(parents=True, exist_ok=True)
    panel = pd.read_csv(args.input_csv)
    estimate_rows: list[dict[str, Any]] = []
    covariance_rows: list[dict[str, Any]] = []
    probes: dict[str, Any] = {}
    schemas: dict[str, Any] = {}
    log_parts = [
        f"F030_PYTHON_INPUT={args.input_csv}",
        f"F030_PYTHON_OUTPUT={args.output_dir}",
        f"PYTHON_VERSION={sys.version}",
    ]

    for name, kwargs, command_suffix in SCENARIOS:
        status, output, error, captured = run_scenario(panel, kwargs)
        log_parts.append(f"SCENARIO={name}")
        if captured:
            log_parts.append(captured)
        probe: dict[str, Any] = {
            "status": status,
            "command": (
                'did_imputation(panel, y="Y", i="unit", t="t", Ei="Ei", '
                f'fe=["unit", "t"], aw="w", minn=0, {command_suffix})'
            ),
        }
        if error is not None:
            probe["error"] = error
        if output is not None:
            estimates = {str(k): float(v) for k, v in output.estimates.items()}
            std_errors = {str(k): float(v) for k, v in output.std_errors.items()}
            probe["terms"] = list(estimates)
            probe["estimates"] = jsonable(estimates)
            probe["std_errors"] = jsonable(std_errors)
            probe["n_obs"] = int(output.n_obs)
            for term, estimate in estimates.items():
                estimate_rows.append(
                    {
                        "scenario": name,
                        "term": term,
                        "estimate": f"{estimate:.17g}",
                        "std_error": f"{std_errors[term]:.17g}",
                        "n_obs": int(output.n_obs),
                    }
                )
            covariance_rows.append(
                {
                    "scenario": name,
                    "row_term": "python_V_total",
                    "col_term": "python_V_total",
                    "value": f"{float(scalar(output.V)):.17g}",
                }
            )
            schemas[name] = {
                "class": type(output).__name__,
                "fields": {
                    field: {
                        "present": hasattr(output, field),
                        "type": type(getattr(output, field, None)).__name__,
                        "is_null": getattr(output, field, None) is None,
                    }
                    for field in [
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
        probes[name] = probe

    write_csv(
        args.output_dir / "estimates.csv",
        estimate_rows,
        ["scenario", "term", "estimate", "std_error", "n_obs"],
    )
    write_csv(
        args.output_dir / "covariance.csv",
        covariance_rows,
        ["scenario", "row_term", "col_term", "value"],
    )
    (args.output_dir / "object-schema.json").write_text(json.dumps(schemas, indent=2) + "\n")
    (args.output_dir / "probes.json").write_text(json.dumps(probes, indent=2) + "\n")
    (args.output_dir / "run.log").write_text("\n".join(log_parts) + "\n")
    diagnostics = {
        "status": "success",
        "python_version": sys.version,
        "platform": platform.platform(),
        "input_sha256": sha256(args.input_csv),
        "generator_sha256": sha256(pathlib.Path(__file__)),
        "scenario_status": {name: probe["status"] for name, probe in probes.items()},
        "source": "pinned Python did_imputation horizon boundary probes",
    }
    (args.output_dir / "diagnostics.json").write_text(json.dumps(diagnostics, indent=2) + "\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
