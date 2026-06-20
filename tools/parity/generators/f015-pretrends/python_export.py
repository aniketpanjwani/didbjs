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
            pretrends=2,
            minn=0,
            cluster="unit",
        )
    message = stdout.getvalue().strip()

    rows = []
    for term, estimate in (output.estimates or {}).items():
        se = output.std_errors[term]
        rows.append({"term": term, "estimate": estimate, "std_error": se, "n_obs": output.n_obs})
    write_csv(output_dir / "estimates.csv", pd.DataFrame(rows))

    pre_rows = []
    for term, estimate in (output.pretrends_estimates or {}).items():
        se = output.pretrends_std_errors[term]
        pre_rows.append({"term": term, "estimate": estimate, "std_error": se, "n_obs": output.n_obs})
    write_csv(output_dir / "pretrends.csv", pd.DataFrame(pre_rows))

    V = np.asarray(output.V)
    names = list((output.estimates or {}).keys()) + list((output.pretrends_estimates or {}).keys())
    covariance_rows = []
    if V.ndim == 0:
        covariance_rows.append({"row_term": "python_V_total", "col_term": "python_V_total", "value": float(V)})
    else:
        for r, row_term in enumerate(names):
            for c, col_term in enumerate(names):
                covariance_rows.append({"row_term": row_term, "col_term": col_term, "value": V[r, c]})
    write_csv(output_dir / "covariance.csv", pd.DataFrame(covariance_rows))

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
        "estimate_names": list((output.estimates or {}).keys()),
        "pretrend_names": list((output.pretrends_estimates or {}).keys()),
    }
    (output_dir / "object-schema.json").write_text(json.dumps(schema, indent=2) + "\n")

    diagnostics = {
        "status": "success",
        "command": 'did_imputation(panel, y="Y", i="unit", t="t", Ei="Ei", fe=["unit", "t"], horizons=[0, 1], pretrends=2, minn=0, cluster="unit")',
        "warning_text": message,
        "n_obs": output.n_obs,
        "terms": names,
    }
    (output_dir / "diagnostics.json").write_text(json.dumps(diagnostics, indent=2) + "\n")
    (output_dir / "run.log").write_text(message + "\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
