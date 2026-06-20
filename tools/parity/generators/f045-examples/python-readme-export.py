#!/usr/bin/env python3
"""Export pinned Python README-style example outputs for F045."""

from __future__ import annotations

import csv
import hashlib
import json
import sys
from pathlib import Path

import matplotlib

matplotlib.use("Agg")

import pandas as pd

from did_imputation import did_imputation, event_plot


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def write_json(path: Path, payload: dict) -> None:
    path.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n")


def rows_from_output(name: str, output) -> list[dict]:
    rows = []
    for term, estimate in (output.estimates or {}).items():
        rows.append(
            {
                "example": name,
                "component": "effect",
                "term": term,
                "estimate": float(estimate),
                "std_error": float(output.std_errors[term]),
            }
        )
    for term, estimate in (output.pretrends_estimates or {}).items():
        rows.append(
            {
                "example": name,
                "component": "pretrend",
                "term": term,
                "estimate": float(estimate),
                "std_error": float(output.pretrends_std_errors[term]),
            }
        )
    return rows


def write_rows(path: Path, rows: list[dict]) -> None:
    with path.open("w", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=["example", "component", "term", "estimate", "std_error"])
        writer.writeheader()
        writer.writerows(rows)


def output_schema(output) -> dict:
    return {
        "class": output.__class__.__name__,
        "has_estimates": output.estimates is not None,
        "has_std_errors": output.std_errors is not None,
        "has_pretrends": output.pretrends_estimates is not None,
        "has_controls": output.controls_estimates is not None,
        "has_weights": output.weights is not None,
        "has_v": output.V is not None,
        "n_obs": int(output.n_obs),
        "estimate_terms": list((output.estimates or {}).keys()),
        "pretrend_terms": list((output.pretrends_estimates or {}).keys()) if output.pretrends_estimates else [],
    }


def main() -> None:
    if len(sys.argv) != 3:
        raise SystemExit("usage: python-readme-export.py <panel_csv> <output_dir>")

    input_path = Path(sys.argv[1])
    output_dir = Path(sys.argv[2])
    output_dir.mkdir(parents=True, exist_ok=True)
    df = pd.read_csv(input_path)

    examples = {
        "readme_static": lambda: did_imputation(df, "Y", "i", "t", "Ei"),
        "readme_allhorizons": lambda: did_imputation(df, "Y", "i", "t", "Ei", allhorizons=True),
        "readme_horizons_0_5": lambda: did_imputation(df, "Y", "i", "t", "Ei", horizons=list(range(0, 5))),
        "readme_sparse_horizons": lambda: did_imputation(df, "Y", "i", "t", "Ei", horizons=[0, 1, 2, 5]),
        "readme_pretrends_3": lambda: did_imputation(df, "Y", "i", "t", "Ei", allhorizons=True, pretrends=3),
    }

    rows: list[dict] = []
    schema = {}
    for name, fn in examples.items():
        output = fn()
        rows.extend(rows_from_output(name, output))
        schema[name] = output_schema(output)

    plot_output = examples["readme_pretrends_3"]()
    fig_path = output_dir / "readme-event-plot.png"
    fig = event_plot(results_obj=plot_output, save_path=str(fig_path), dpi=72)
    schema["readme_event_plot"] = {
        "figure_class": type(fig).__name__,
        "axes_count": len(fig.axes),
        "saved": fig_path.exists() and fig_path.stat().st_size > 0,
        "size": fig_path.stat().st_size,
        "sha256": sha256(fig_path),
    }

    write_rows(output_dir / "estimates.csv", rows)
    write_json(output_dir / "output-schema.json", schema)
    write_json(
        output_dir / "diagnostics.json",
        {
            "status": "success",
            "source": "pinned Python README usage examples",
            "input_sha256": sha256(input_path),
            "generator_sha256": sha256(Path(__file__)),
            "examples": list(examples.keys()) + ["readme_event_plot"],
            "readme_examples_inventory": {
                "did_imputation_static": "run",
                "dynamic_allhorizons": "run",
                "dynamic_horizons_range": "run",
                "dynamic_sparse_horizons": "run",
                "pretrends": "run",
                "event_plot_results_obj": "run",
                "complex_fe_region_triple_diff_and_wtr": "covered by earlier dedicated fixtures F008/F010/F011/F029 rather than duplicated here",
            },
        },
    )
    print("F045_PYTHON_README_EXPORT_OK=1")


if __name__ == "__main__":
    main()
