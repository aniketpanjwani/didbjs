#!/usr/bin/env python3
"""Generate the F043 eight-model event_plot semantic fixture."""

from __future__ import annotations

import csv
import hashlib
import json
import platform
import sys
from datetime import datetime, timezone
from pathlib import Path
from statistics import NormalDist


ROOT = Path(__file__).resolve().parents[4]
STATA_COMMIT = "767c8d6670a751170910d419bbafd323df92ef08"
PYTHON_COMMIT = "c7765a9fb2dcc48dc745b356784b4e9ce8b1d376"


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def rounded(value: float) -> float:
    return float(f"{value:.15g}")


def make_payload() -> dict:
    model_names = [f"Spec {chr(ord('A') + idx)}" for idx in range(8)]
    models = []
    for idx, name in enumerate(model_names):
        base = idx + 1
        estimates = {
            "pre3": rounded(-0.45 - 0.025 * base),
            "pre2": rounded(-0.30 - 0.020 * base),
            "pre1": rounded(-0.12 - 0.010 * base),
            "tau0": rounded(0.70 + 0.140 * base),
            "tau1": rounded(1.00 + 0.180 * base),
            "tau2": rounded(1.25 + 0.210 * base),
        }
        std_errors = {
            "pre3": rounded(0.050 + 0.003 * base),
            "pre2": rounded(0.060 + 0.004 * base),
            "pre1": rounded(0.070 + 0.005 * base),
            "tau0": rounded(0.100 + 0.006 * base),
            "tau1": rounded(0.120 + 0.007 * base),
            "tau2": rounded(0.150 + 0.008 * base),
        }
        models.append({"name": name, "estimates": estimates, "std_errors": std_errors})

    return {
        "fixture_id": "F043",
        "description": "Eight-model Stata-like event_plot overlay fixture with scalar stubs/trims and per-model shifts/perturbations.",
        "significance_level": 0.05,
        "stub_lag": "tau#",
        "stub_lead": "pre#",
        "trimlag": 1,
        "trimlead": 2,
        "shift": [0.00, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70],
        "perturb": [-0.28, -0.20, -0.12, -0.04, 0.04, 0.12, 0.20, 0.28],
        "model_names": model_names,
        "plot_type": "rcap",
        "together": False,
        "models": models,
    }


def event_time(term: str) -> int:
    if term.startswith("pre"):
        return -int(term.removeprefix("pre"))
    if term.startswith("tau"):
        return int(term.removeprefix("tau"))
    raise ValueError(f"unexpected term: {term}")


def expected_rows(payload: dict) -> list[dict]:
    critical = NormalDist().inv_cdf(1 - payload["significance_level"] / 2)
    rows: list[dict] = []
    for model_index, model in enumerate(payload["models"], start=1):
        kept_terms = []
        for term in model["estimates"]:
            time = event_time(term)
            if time >= 0 and time > payload["trimlag"]:
                continue
            if time < 0 and abs(time) > payload["trimlead"]:
                continue
            kept_terms.append((term, time))
        kept_terms.sort(key=lambda item: item[1])
        for term, time in kept_terms:
            estimate = float(model["estimates"][term])
            std_error = float(model["std_errors"][term])
            error = critical * std_error
            model_label = payload["model_names"][model_index - 1]
            series = "Effects" if time >= 0 else "Pre-trends"
            rows.append(
                {
                    "source": "stata",
                    "plot_type": payload["plot_type"],
                    "together": "FALSE",
                    "series": series,
                    "term": term,
                    "event_time": time,
                    "estimate": estimate,
                    "std_error": std_error,
                    "critical_value": critical,
                    "ci_low": estimate - error,
                    "ci_high": estimate + error,
                    "has_ci": "TRUE",
                    "model": model_index,
                    "model_label": model_label,
                    "position": time + payload["perturb"][model_index - 1] - payload["shift"][model_index - 1],
                    "plot_group_label": f"{model_label}|{series}",
                }
            )
    return rows


def write_csv(path: Path, rows: list[dict]) -> None:
    fields = [
        "source",
        "plot_type",
        "together",
        "series",
        "term",
        "event_time",
        "estimate",
        "std_error",
        "critical_value",
        "ci_low",
        "ci_high",
        "has_ci",
        "model",
        "model_label",
        "position",
        "plot_group_label",
    ]
    with path.open("w", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fields)
        writer.writeheader()
        writer.writerows(rows)


def write_json(path: Path, payload: dict) -> None:
    path.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n")


def maybe_hash(paths: list[Path]) -> dict[str, str]:
    out = {}
    for path in paths:
        resolved = path.resolve()
        if resolved.exists():
            out[str(resolved.relative_to(ROOT))] = sha256(resolved)
    return out


def main() -> None:
    if len(sys.argv) != 2:
        raise SystemExit("usage: semantic_export.py <fixture_dir>")

    fixture_dir = Path(sys.argv[1]).resolve()
    input_dir = fixture_dir / "inputs"
    expected_dir = fixture_dir / "expected" / "semantic"
    metadata_dir = fixture_dir / "metadata"
    input_dir.mkdir(parents=True, exist_ok=True)
    expected_dir.mkdir(parents=True, exist_ok=True)
    metadata_dir.mkdir(parents=True, exist_ok=True)

    payload = make_payload()
    rows = expected_rows(payload)
    input_path = input_dir / "models.json"
    plot_data_path = expected_dir / "plot-data.csv"
    diagnostics_path = expected_dir / "diagnostics.json"
    manifest_path = metadata_dir / "manifest.json"

    write_json(input_path, payload)
    write_csv(plot_data_path, rows)
    write_json(
        diagnostics_path,
        {
            "status": "success",
            "fixture_id": "F043",
            "model_count": len(payload["models"]),
            "rows": len(rows),
            "max_supported_models": 8,
            "trimlag": payload["trimlag"],
            "trimlead": payload["trimlead"],
            "shift": payload["shift"],
            "perturb": payload["perturb"],
            "critical_value": NormalDist().inv_cdf(1 - payload["significance_level"] / 2),
            "scalar_arguments_recycled": ["stub_lag", "stub_lead", "trimlag", "trimlead"],
            "legend_order": payload["model_names"],
            "style_recycling_scope": "D013 semantic R plot-data and scalar option recycling; literal Stata graph styles are excluded.",
        },
    )

    hashed_paths = [
        input_path,
        plot_data_path,
        diagnostics_path,
        ROOT / "tests/testthat/test-f043-plot-multimodel.R",
        Path(__file__),
    ]
    write_json(
        manifest_path,
        {
            "fixture_id": "F043",
            "profile": "conformance-profile-v1",
            "decision_record_id": "D013",
            "generated_at": datetime.now(timezone.utc).replace(microsecond=0).isoformat().replace("+00:00", "Z"),
            "host": platform.node(),
            "input_dir": "tests/fixtures/parity/f043-plot-multimodel/inputs",
            "commands": {
                "semantic": "python3 tools/parity/generators/f043-plot-multimodel/semantic_export.py tests/fixtures/parity/f043-plot-multimodel",
                "focused_test": "R_LIBS_USER=.r-lib Rscript -e 'testthat::test_local(filter = \"f043|f023|f022\", reporter = \"summary\", stop_on_failure = TRUE)'",
            },
            "reference_commits": {
                "stata": STATA_COMMIT,
                "python": PYTHON_COMMIT,
                "semantic_oracle": "independent D013 translation of F023 savecoef coordinates to eight-model overlay semantics",
            },
            "terminal_status": {
                "semantic": "success",
            },
            "sha256": maybe_hash(hashed_paths),
        },
    )
    print("F043_SEMANTIC_EXPORT_OK=1")


if __name__ == "__main__":
    main()
