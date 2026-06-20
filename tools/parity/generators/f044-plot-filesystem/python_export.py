#!/usr/bin/env python3
"""Generate F044 event_plot filesystem/output reference artifacts."""

from __future__ import annotations

import hashlib
import json
import platform
import shutil
import struct
import sys
import tempfile
from datetime import datetime, timezone
from pathlib import Path

import matplotlib

matplotlib.use("Agg")

import matplotlib.pyplot as plt

from did_imputation import event_plot


ROOT = Path(__file__).resolve().parents[4]
PYTHON_COMMIT = "c7765a9fb2dcc48dc745b356784b4e9ce8b1d376"
STATA_COMMIT = "767c8d6670a751170910d419bbafd323df92ef08"


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def write_json(path: Path, payload: dict) -> None:
    path.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n")


def png_dimensions(path: Path) -> list[int]:
    raw = path.read_bytes()
    if raw[:8] != b"\x89PNG\r\n\x1a\n":
        raise ValueError(f"{path} is not a PNG")
    width, height = struct.unpack(">II", raw[16:24])
    return [int(width), int(height)]


def make_payload() -> dict:
    return {
        "fixture_id": "F044",
        "pretrends": {
            "pre2": -0.32,
            "pre1": -0.11,
        },
        "pretrends_std": {
            "pre2": 0.08,
            "pre1": 0.07,
        },
        "effects": {
            "tau0": 0.74,
            "tau1": 1.08,
        },
        "effects_std": {
            "tau0": 0.10,
            "tau1": 0.12,
        },
        "significance_level": 0.05,
        "plot_type": "rcap",
        "figsize": [4, 3],
        "dpi": 72,
        "expected_png_pixels": [288, 216],
        "title": "F044 filesystem probe",
        "xlabel": "Relative time",
        "ylabel": "Estimate",
    }


def run_python_reference(payload: dict, output_dir: Path) -> dict:
    png_path = output_dir / "python-rcap.png"
    overwrite_path = output_dir / "python-overwrite.png"
    invalid_path = output_dir / "missing-parent" / "plot.png"
    overwrite_path.write_text("sentinel that should be replaced by matplotlib\n")

    fig = event_plot(
        pretrends=payload["pretrends"],
        pretrends_std=payload["pretrends_std"],
        effects=payload["effects"],
        effects_std=payload["effects_std"],
        significance_level=payload["significance_level"],
        plot_type=payload["plot_type"],
        figsize=tuple(payload["figsize"]),
        title=payload["title"],
        xlabel=payload["xlabel"],
        ylabel=payload["ylabel"],
        save_path=str(png_path),
        dpi=payload["dpi"],
    )
    overwrite_fig = event_plot(
        pretrends=payload["pretrends"],
        pretrends_std=payload["pretrends_std"],
        effects=payload["effects"],
        effects_std=payload["effects_std"],
        significance_level=payload["significance_level"],
        plot_type=payload["plot_type"],
        figsize=tuple(payload["figsize"]),
        save_path=str(overwrite_path),
        dpi=payload["dpi"],
    )

    invalid_error = None
    try:
        event_plot(
            pretrends=payload["pretrends"],
            pretrends_std=payload["pretrends_std"],
            effects=payload["effects"],
            effects_std=payload["effects_std"],
            significance_level=payload["significance_level"],
            plot_type=payload["plot_type"],
            figsize=tuple(payload["figsize"]),
            save_path=str(invalid_path),
            dpi=payload["dpi"],
        )
    except Exception as exc:  # noqa: BLE001 - artifact records upstream class/text
        invalid_error = {
            "class": exc.__class__.__name__,
            "message": str(exc),
        }

    diagnostics = {
        "status": "success",
        "fixture_id": "F044",
        "matplotlib_backend": matplotlib.get_backend(),
        "figure_class": type(fig).__name__,
        "figure_inches": [float(x) for x in fig.get_size_inches()],
        "dpi": payload["dpi"],
        "png_exists": png_path.exists(),
        "png_sha256": sha256(png_path),
        "png_size": png_path.stat().st_size,
        "png_magic": png_path.read_bytes()[:8].hex(),
        "png_dimensions": png_dimensions(png_path),
        "overwrite_existing_path": True,
        "overwrite_size": overwrite_path.stat().st_size,
        "overwrite_sha256": sha256(overwrite_path),
        "invalid_parent_error": invalid_error,
        "open_figures_before_close": sorted(int(x) for x in plt.get_fignums()),
    }
    plt.close(fig)
    plt.close(overwrite_fig)
    plt.close("all")
    diagnostics["open_figures_after_close"] = sorted(int(x) for x in plt.get_fignums())
    return diagnostics


def artifact_hashes(paths: list[Path]) -> dict[str, str]:
    out = {}
    for path in paths:
        resolved = path.resolve()
        if resolved.exists():
            out[str(resolved.relative_to(ROOT))] = sha256(resolved)
    return out


def main() -> None:
    if len(sys.argv) != 2:
        raise SystemExit("usage: python_export.py <fixture_dir>")

    fixture_dir = Path(sys.argv[1]).resolve()
    input_dir = fixture_dir / "inputs"
    python_dir = fixture_dir / "expected" / "python"
    semantic_dir = fixture_dir / "expected" / "semantic"
    metadata_dir = fixture_dir / "metadata"
    for path in (input_dir, python_dir, semantic_dir, metadata_dir):
        path.mkdir(parents=True, exist_ok=True)

    payload = make_payload()
    input_path = input_dir / "manual.json"
    diagnostics_path = python_dir / "diagnostics.json"
    policy_path = semantic_dir / "policy.json"
    manifest_path = metadata_dir / "manifest.json"
    write_json(input_path, payload)

    with tempfile.TemporaryDirectory() as tmp:
        tmp_dir = Path(tmp)
        diagnostics = run_python_reference(payload, tmp_dir)
        shutil.copyfile(tmp_dir / "python-rcap.png", python_dir / "python-rcap.png")
        write_json(diagnostics_path, diagnostics)

    write_json(
        policy_path,
        {
            "fixture_id": "F044",
            "status": "success",
            "policy": "D013 semantic R plotting translation",
            "headless_rendering": "event_plot returns a ggplot object without requiring an interactive device",
            "file_type": "PNG output uses the PNG magic bytes and configured dimensions",
            "dimensions": {
                "figsize": payload["figsize"],
                "dpi": payload["dpi"],
                "expected_png_pixels": payload["expected_png_pixels"],
            },
            "overwrite_policy": {
                "python_reference": "matplotlib savefig overwrites an existing path",
                "didbjs": "overwrite = FALSE fails closed before rendering",
            },
            "invalid_paths": {
                "empty": "structured didbjs_contract_error",
                "directory": "structured didbjs_contract_error",
                "missing_parent": "structured didbjs_contract_error",
            },
            "noplot_policy": "noplot returns plot_data with plot = NULL and cannot be combined with save_path",
            "device_policy": "successful saves and validation failures must leave R device state unchanged",
            "theme_policy": "event_plot must not mutate ggplot2::theme_get()",
        },
    )

    hashed = [
        input_path,
        diagnostics_path,
        policy_path,
        python_dir / "python-rcap.png",
        ROOT / "tests/testthat/test-f044-plot-filesystem.R",
        Path(__file__),
    ]
    write_json(
        manifest_path,
        {
            "fixture_id": "F044",
            "profile": "conformance-profile-v1",
            "decision_record_id": "D013",
            "generated_at": datetime.now(timezone.utc).replace(microsecond=0).isoformat().replace("+00:00", "Z"),
            "host": platform.node(),
            "input_dir": "tests/fixtures/parity/f044-plot-filesystem/inputs",
            "commands": {
                "python": ".venv/bin/python tools/parity/generators/f044-plot-filesystem/python_export.py tests/fixtures/parity/f044-plot-filesystem",
                "focused_test": "R_LIBS_USER=.r-lib Rscript -e 'testthat::test_local(filter = \"f044|f022|f042|f043\", reporter = \"summary\", stop_on_failure = TRUE)'",
            },
            "reference_commits": {
                "python": PYTHON_COMMIT,
                "stata": STATA_COMMIT,
                "semantic_profile": "D013 plus F022/F023 Stata noplot/savecoef evidence",
            },
            "terminal_status": {
                "python": "success",
            },
            "sha256": artifact_hashes(hashed),
        },
    )
    print("F044_PYTHON_EXPORT_OK=1")


if __name__ == "__main__":
    main()
