#!/usr/bin/env python3
import contextlib
import io
import json
import sys
from pathlib import Path

import numpy as np
import pandas as pd

from did_imputation import did_imputation


def write_csv(path: Path, frame: pd.DataFrame) -> None:
    frame.to_csv(path, index=False)


def main() -> int:
    if len(sys.argv) < 3:
        print("usage: python_export.py <input_csv> <output_dir>", file=sys.stderr)
        return 2

    input_csv = Path(sys.argv[1])
    output_dir = Path(sys.argv[2])
    output_dir.mkdir(parents=True, exist_ok=True)

    panel = pd.read_csv(input_csv)

    stdout = io.StringIO()
    with contextlib.redirect_stdout(stdout):
        output = did_imputation(
            panel,
            y="Y",
            i="unit",
            t="t",
            Ei="Ei",
            fe=["unit", "t"],
            horizons=[0, 1],
            minn=30,
            cluster="unit",
        )
    message = stdout.getvalue().strip()

    estimates = output.estimates or {}
    std_errors = output.std_errors or {}
    rows = []
    for term, estimate in estimates.items():
        se = std_errors.get(term, np.nan)
        rows.append(
            {
                "term": term,
                "estimate": estimate,
                "std_error": se,
                "conf_low": estimate - 1.959963984540054 * se if pd.notna(se) else np.nan,
                "conf_high": estimate + 1.959963984540054 * se if pd.notna(se) else np.nan,
                "n_obs": output.n_obs,
                "suppressed": int(f"wtr{term.replace('tau', '')}" in message),
            }
        )
    write_csv(output_dir / "estimates.csv", pd.DataFrame(rows))
    write_csv(
        output_dir / "covariance.csv",
        pd.DataFrame(
            [
                {
                    "row_term": "python_V_total",
                    "col_term": "python_V_total",
                    "value": float(np.asarray(output.V).squeeze()),
                }
            ]
        ),
    )

    schema = {
        "class": type(output).__name__,
        "fields": {
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
        },
        "estimate_names": list(estimates.keys()),
        "std_error_names": list(std_errors.keys()) if isinstance(std_errors, dict) else [],
    }
    (output_dir / "object-schema.json").write_text(json.dumps(schema, indent=2) + "\n")

    diagnostics = {
        "status": "success",
        "command": 'did_imputation(panel, y="Y", i="unit", t="t", Ei="Ei", fe=["unit", "t"], horizons=[0, 1], minn=30, cluster="unit")',
        "minn": 30,
        "warning_text": message,
        "n_obs": output.n_obs,
        "terms": list(estimates.keys()),
    }
    (output_dir / "diagnostics.json").write_text(json.dumps(diagnostics, indent=2) + "\n")
    (output_dir / "run.log").write_text(message + "\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
