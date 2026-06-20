#!/usr/bin/env python3
"""Record pinned Python event_plot validation behavior for F042."""

from __future__ import annotations

import hashlib
import json
import math
import pathlib
import sys
import traceback
from typing import Any

import matplotlib

matplotlib.use("Agg")

import matplotlib.pyplot as plt

from did_imputation import event_plot


def sha256(path: pathlib.Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def run_probe(name: str, kwargs: dict[str, Any]) -> dict[str, Any]:
    try:
        fig = event_plot(**kwargs)
        status = {
            "status": "success",
            "figure_class": type(fig).__name__,
            "axes_count": len(fig.axes),
        }
        plt.close(fig)
        return status
    except Exception as exc:  # noqa: BLE001 - reference exporter records raw upstream failures.
        return {
            "status": "error",
            "error_class": type(exc).__name__,
            "error_message": str(exc),
            "traceback_tail": traceback.format_exc().strip().splitlines()[-3:],
        }


def main() -> int:
    if len(sys.argv) != 3:
        raise SystemExit("usage: python_export.py <manual_json> <output_dir>")
    input_path = pathlib.Path(sys.argv[1])
    output_dir = pathlib.Path(sys.argv[2])
    output_dir.mkdir(parents=True, exist_ok=True)

    payload = json.loads(input_path.read_text())
    base = {
        "pretrends": payload["pretrends"],
        "pretrends_std": payload["pretrends_std"],
        "effects": payload["effects"],
        "effects_std": payload["effects_std"],
        "significance_level": payload["significance_level"],
    }

    probes = {
        "missing_pretrend_se": {
            **base,
            "pretrends_std": {"pre1": payload["pretrends_std"]["pre1"]},
        },
        "extra_effect_se": {
            **base,
            "effects_std": {**payload["effects_std"], "tau9": 0.9},
        },
        "std_without_estimates": {
            "pretrends_std": {"pre1": 0.05},
            "effects": payload["effects"],
            "effects_std": payload["effects_std"],
        },
        "bad_term_name": {
            **base,
            "effects": {"bad0": 1.0},
            "effects_std": {"bad0": 0.1},
        },
        "nan_estimate": {
            **base,
            "effects": {"tau0": math.nan},
            "effects_std": {"tau0": 0.1},
        },
        "inf_std_error": {
            **base,
            "effects_std": {"tau0": math.inf, "tau1": 0.2, "tau2": 0.3},
        },
        "alpha_zero": {
            **base,
            "significance_level": 0,
        },
        "alpha_one": {
            **base,
            "significance_level": 1,
        },
        "unsupported_kwarg": {
            **base,
            "definitely_not_supported_by_matplotlib": 1,
        },
        "object_plus_manual": {
            **base,
            "results_obj": object(),
        },
    }

    results = {name: run_probe(name, kwargs) for name, kwargs in probes.items()}
    diagnostics = {
        "status": "success",
        "matplotlib_version": matplotlib.__version__,
        "input_sha256": sha256(input_path),
        "generator_sha256": sha256(pathlib.Path(__file__)),
        "probe_count": len(results),
        "notes": {
            "duplicate_terms": "Python dictionaries cannot represent duplicate term names; didbjs validates duplicate named R vectors directly.",
            "extra_effect_se": "Pinned Python ignores extra SE names because it indexes only estimate keys; didbjs fails closed under D013/F042.",
            "alpha_limits": "Pinned Python accepts alpha boundary values and produces degenerate/non-finite intervals; didbjs fails closed under D013/F042.",
        },
    }

    (output_dir / "probes.json").write_text(json.dumps(results, indent=2, allow_nan=True) + "\n")
    (output_dir / "diagnostics.json").write_text(json.dumps(diagnostics, indent=2) + "\n")
    print("F042_PYTHON_EXPORT_OK=1")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
