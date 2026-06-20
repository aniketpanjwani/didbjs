#!/usr/bin/env python3
"""Write F045 source inventory and manifest hashes."""

from __future__ import annotations

import hashlib
import json
import platform
import sys
from datetime import datetime, timezone
from pathlib import Path


ROOT = Path(__file__).resolve().parents[4]
STATA_COMMIT = "767c8d6670a751170910d419bbafd323df92ef08"
PYTHON_COMMIT = "c7765a9fb2dcc48dc745b356784b4e9ce8b1d376"
KYLE_COMMIT = "69b4f8dfe16b007474721fc5610859b56a80cdc6"


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def write_json(path: Path, payload: dict) -> None:
    path.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n")


def existing_hashes(paths: list[Path]) -> dict[str, str]:
    out = {}
    for path in paths:
        resolved = path.resolve()
        if resolved.exists():
            out[str(resolved.relative_to(ROOT))] = sha256(resolved)
    return out


def main() -> None:
    if len(sys.argv) != 2:
        raise SystemExit("usage: write-manifest.py <fixture_dir>")

    fixture_dir = Path(sys.argv[1]).resolve()
    metadata_dir = fixture_dir / "metadata"
    metadata_dir.mkdir(parents=True, exist_ok=True)

    source_paths = {
        "stata_five_estimators_example": Path("<reference-clone-root>/did_imputation_stata/five_estimators_example.do"),
        "python_readme": Path("<reference-clone-root>/did_imputation_python/README.md"),
        "python_event_plot": Path("<reference-clone-root>/did_imputation_python/src/did_imputation/event_plot.py"),
        "python_estimator": Path("<reference-clone-root>/did_imputation_python/src/did_imputation/did_imputation.py"),
        "kyle_readme": Path("<reference-clone-root>/didimputation_r_kyle/README.md"),
        "kyle_estimator": Path("<reference-clone-root>/didimputation_r_kyle/R/did_imputation.R"),
        "kyle_data_docs": Path("<reference-clone-root>/didimputation_r_kyle/R/data.R"),
    }
    source_inventory = {
        "fixture_id": "F045",
        "source_boundary": {
            "stata": "Curates the BJS did_imputation/event_plot portion of five_estimators_example.do; third-party estimators in that script are inventoried but not reimplemented by didbjs.",
            "python": "Runs pinned README did_imputation/event_plot examples whose inputs are available offline; complex-design snippets are covered by earlier dedicated fixtures.",
            "kyle": "Runs pinned README/package static and explicit event-study examples; horizon = TRUE is recorded as a D023 approved divergence for didbjs RC-v1.",
        },
        "source_sha256": {name: sha256(path) for name, path in source_paths.items()},
        "reference_commits": {
            "stata": STATA_COMMIT,
            "python": PYTHON_COMMIT,
            "kyle": KYLE_COMMIT,
        },
    }
    source_inventory_path = metadata_dir / "source-inventory.json"
    write_json(source_inventory_path, source_inventory)

    hashed = [
        fixture_dir / "inputs" / "stata-five-bjs-panel.csv",
        fixture_dir / "inputs" / "kyle-df-het.csv",
        fixture_dir / "expected" / "stata" / "diagnostics.json",
        fixture_dir / "expected" / "stata" / "estimates.csv",
        fixture_dir / "expected" / "stata" / "covariance.csv",
        fixture_dir / "expected" / "stata" / "plot-data.csv",
        fixture_dir / "expected" / "stata" / "true-effects.csv",
        fixture_dir / "expected" / "stata" / "run.log",
        fixture_dir / "expected" / "python" / "diagnostics.json",
        fixture_dir / "expected" / "python" / "estimates.csv",
        fixture_dir / "expected" / "python" / "output-schema.json",
        fixture_dir / "expected" / "python" / "readme-event-plot.png",
        fixture_dir / "expected" / "kyle" / "diagnostics.json",
        fixture_dir / "expected" / "kyle" / "estimates.csv",
        fixture_dir / "expected" / "kyle" / "output-schema.json",
        source_inventory_path,
        ROOT / "tests" / "testthat" / "test-f045-examples.R",
        ROOT / "tools" / "parity" / "generators" / "f045-examples" / "stata-five-bjs.do",
        ROOT / "tools" / "parity" / "generators" / "f045-examples" / "python-readme-export.py",
        ROOT / "tools" / "parity" / "generators" / "f045-examples" / "kyle-export.R",
        Path(__file__),
    ]
    manifest = {
        "fixture_id": "F045",
        "profile": "conformance-profile-v1",
        "decision_record_ids": ["D015", "D017", "D023"],
        "generated_at": datetime.now(timezone.utc).replace(microsecond=0).isoformat().replace("+00:00", "Z"),
        "host": platform.node(),
        "input_dir": "tests/fixtures/parity/f045-examples/inputs",
        "commands": {
            "stata": "ssh <licensed-stata-host> 'cd <remote_tmp> && /usr/local/bin/stata -b do stata-five-bjs.do <remote_tmp>/out ${STATA_ADO_ROOT}'",
            "python": ".venv/bin/python tools/parity/generators/f045-examples/python-readme-export.py tests/fixtures/parity/f045-examples/inputs/stata-five-bjs-panel.csv tests/fixtures/parity/f045-examples/expected/python",
            "kyle": "R_LIBS_USER=.r-lib Rscript tools/parity/generators/f045-examples/kyle-export.R tests/fixtures/parity/f045-examples/inputs tests/fixtures/parity/f045-examples/expected/kyle",
            "manifest": "python3 tools/parity/generators/f045-examples/write-manifest.py tests/fixtures/parity/f045-examples",
            "focused_test": "R_LIBS_USER=.r-lib Rscript -e 'testthat::test_local(filter = \"f045\", reporter = \"summary\", stop_on_failure = TRUE)'",
        },
        "reference_commits": {
            "stata": STATA_COMMIT,
            "python": PYTHON_COMMIT,
            "kyle": KYLE_COMMIT,
        },
        "terminal_status": {
            "stata": "success",
            "python": "success",
            "kyle": "success_with_d023_horizon_true_divergence",
            "focused_test": "success",
        },
        "sha256": existing_hashes(hashed),
    }
    write_json(metadata_dir / "manifest.json", manifest)
    print("F045_MANIFEST_OK=1")


if __name__ == "__main__":
    main()
