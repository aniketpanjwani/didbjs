#!/usr/bin/env python3

import csv
import hashlib
import json
import sys
from pathlib import Path

import matplotlib

matplotlib.use("Agg")

import matplotlib.pyplot as plt
import scipy
from scipy import stats

from did_imputation import DIDImputationOutput, event_plot


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def write_csv(path: Path, rows, fieldnames):
    with path.open("w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        for row in rows:
            writer.writerow(row)


def plot_rows(payload, source, plot_type, together):
    critical = stats.norm.ppf(1 - payload["significance_level"] / 2)
    rows = []
    for term, estimate in payload["pretrends"].items():
        se = payload["pretrends_std"][term]
        error = critical * se
        rows.append(
            {
                "source": source,
                "plot_type": plot_type,
                "together": str(together).upper(),
                "series": "Effects" if together else "Pre-trends",
                "term": term,
                "event_time": -int(term.replace("pre", "")),
                "estimate": float(estimate),
                "std_error": float(se),
                "critical_value": critical,
                "ci_low": float(estimate) - error,
                "ci_high": float(estimate) + error,
                "has_ci": "TRUE",
            }
        )
    for term, estimate in payload["effects"].items():
        se = payload["effects_std"][term]
        error = critical * se
        rows.append(
            {
                "source": source,
                "plot_type": plot_type,
                "together": str(together).upper(),
                "series": "Effects",
                "term": term,
                "event_time": int(term.replace("tau", "")),
                "estimate": float(estimate),
                "std_error": float(se),
                "critical_value": critical,
                "ci_low": float(estimate) - error,
                "ci_high": float(estimate) + error,
                "has_ci": "TRUE",
            }
        )
    rows.sort(key=lambda row: row["event_time"])
    return rows


def main():
    if len(sys.argv) != 3:
        raise SystemExit("usage: python_export.py <manual_json> <output_dir>")
    input_path = Path(sys.argv[1])
    output_dir = Path(sys.argv[2])
    output_dir.mkdir(parents=True, exist_ok=True)

    payload = json.loads(input_path.read_text())

    object_payload = DIDImputationOutput(
        pretrends_estimates=payload["pretrends"],
        pretrends_std_errors=payload["pretrends_std"],
        estimates=payload["effects"],
        std_errors=payload["effects_std"],
    )

    manual_rcap_path = output_dir / "manual-rcap.png"
    manual_rarea_path = output_dir / "manual-rarea.png"

    fig_manual_rcap = event_plot(
        pretrends=payload["pretrends"],
        pretrends_std=payload["pretrends_std"],
        effects=payload["effects"],
        effects_std=payload["effects_std"],
        significance_level=payload["significance_level"],
        plot_type="rcap",
        save_path=str(manual_rcap_path),
    )
    fig_manual_rarea = event_plot(
        pretrends=payload["pretrends"],
        pretrends_std=payload["pretrends_std"],
        effects=payload["effects"],
        effects_std=payload["effects_std"],
        significance_level=payload["significance_level"],
        plot_type="rarea",
        save_path=str(manual_rarea_path),
    )
    fig_object = event_plot(
        results_obj=object_payload,
        significance_level=payload["significance_level"],
        plot_type="rcap",
    )
    fig_together = event_plot(
        results_obj=object_payload,
        significance_level=payload["significance_level"],
        plot_type="rcap",
        together=True,
    )

    rows = []
    rows.extend(plot_rows(payload, source="manual", plot_type="rcap", together=False))
    rows.extend(plot_rows(payload, source="manual", plot_type="rarea", together=False))
    rows.extend(plot_rows(payload, source="object", plot_type="rcap", together=False))
    rows.extend(plot_rows(payload, source="object", plot_type="rcap", together=True))
    write_csv(
        output_dir / "plot-data.csv",
        rows,
        [
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
        ],
    )

    schema = {
        "figure_class": type(fig_manual_rcap).__name__,
        "axes_count": len(fig_manual_rcap.axes),
        "manual_rcap_lines": len(fig_manual_rcap.axes[0].lines),
        "manual_rarea_collections": len(fig_manual_rarea.axes[0].collections),
        "object_lines": len(fig_object.axes[0].lines),
        "together_lines": len(fig_together.axes[0].lines),
    }
    diagnostics = {
        "status": "success",
        "matplotlib_version": matplotlib.__version__,
        "scipy_version": scipy.__version__,
        "input_sha256": sha256(input_path),
        "generator_sha256": sha256(Path(__file__)),
        "manual_rcap_saved": manual_rcap_path.exists() and manual_rcap_path.stat().st_size > 0,
        "manual_rarea_saved": manual_rarea_path.exists() and manual_rarea_path.stat().st_size > 0,
        "manual_rcap_size": manual_rcap_path.stat().st_size,
        "manual_rarea_size": manual_rarea_path.stat().st_size,
        "critical_value": stats.norm.ppf(1 - payload["significance_level"] / 2),
    }

    (output_dir / "output-schema.json").write_text(json.dumps(schema, indent=2) + "\n")
    (output_dir / "diagnostics.json").write_text(json.dumps(diagnostics, indent=2) + "\n")

    plt.close("all")
    print("F022_PYTHON_EXPORT_OK=1")


if __name__ == "__main__":
    main()
