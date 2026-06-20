#!/usr/bin/env python3
"""Generate the hand-solvable F036 algebraic oracle for the F001 panel."""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
import os
import pathlib
from decimal import Decimal, getcontext


getcontext().prec = 28


def sha256(path: pathlib.Path) -> str:
    h = hashlib.sha256()
    with path.open("rb") as f:
        for chunk in iter(lambda: f.read(1024 * 1024), b""):
            h.update(chunk)
    return h.hexdigest()


def as_decimal(value: str) -> Decimal | None:
    if value == "":
        return None
    return Decimal(value)


def write_csv(path: pathlib.Path, rows: list[dict[str, object]], fields: list[str]) -> None:
    with path.open("w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fields)
        writer.writeheader()
        writer.writerows(rows)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("input_csv", type=pathlib.Path)
    parser.add_argument("metadata_dir", type=pathlib.Path)
    args = parser.parse_args()

    args.metadata_dir.mkdir(parents=True, exist_ok=True)
    rows: list[dict[str, object]] = []
    with args.input_csv.open(newline="") as f:
      reader = csv.DictReader(f)
      for raw in reader:
          unit = int(raw["i"])
          period = int(raw["t"])
          event_time = None if raw["event_time"] == "" else int(raw["event_time"])
          treatment_time = None if raw["Ei"] == "" else int(raw["Ei"])
          d = int(raw["D"])
          y0 = as_decimal(raw["Y0"])
          tau = as_decimal(raw["tau"])
          y = as_decimal(raw["Y"])
          w = as_decimal(raw["w"])
          expected_y0 = Decimal(10 * unit + period)
          if y0 != expected_y0:
              raise AssertionError(f"Y0 formula failed for {raw['row_id']}: {y0} != {expected_y0}")
          if d == 1:
              if treatment_time != 4 or event_time not in {0, 1, 2}:
                  raise AssertionError(f"treated timing failed for {raw['row_id']}")
              expected_tau = Decimal(1 + event_time) + (Decimal(unit - 3) / Decimal(10))
          else:
              expected_tau = Decimal(0)
          if tau != expected_tau:
              raise AssertionError(f"tau formula failed for {raw['row_id']}: {tau} != {expected_tau}")
          if y != y0 + tau:
              raise AssertionError(f"Y formula failed for {raw['row_id']}")
          if w != Decimal(1):
              raise AssertionError(f"analytic weight is not one for {raw['row_id']}")
          rows.append(
              {
                  "row_id": raw["row_id"],
                  "unit": unit,
                  "period": period,
                  "treatment_time": treatment_time,
                  "event_time": event_time,
                  "D": d,
                  "Y0": y0,
                  "tau": tau,
                  "Y": y,
              }
          )

    if len(rows) != 60:
        raise AssertionError(f"expected 60 rows, found {len(rows)}")
    units = sorted({row["unit"] for row in rows})
    periods = sorted({row["period"] for row in rows})
    if units != list(range(1, 11)) or periods != list(range(1, 7)):
        raise AssertionError("F001 panel support changed")

    treated_rows = [row for row in rows if row["D"] == 1]
    control_rows = [row for row in rows if row["D"] == 0]
    treated_count = len(treated_rows)
    if treated_count != 15 or len(control_rows) != 45:
        raise AssertionError("F001 treated/control counts changed")

    static_weight = Decimal(1) / Decimal(treated_count)
    static_att = sum(row["tau"] * static_weight for row in treated_rows)
    horizon_rows = []
    weight_rows = []
    horizon_effects: dict[str, Decimal] = {}
    for horizon in sorted({row["event_time"] for row in treated_rows}):
        horizon_treated = [row for row in treated_rows if row["event_time"] == horizon]
        horizon_weight = Decimal(1) / Decimal(len(horizon_treated))
        term = f"tau{horizon}"
        effect = sum(row["tau"] * horizon_weight for row in horizon_treated)
        horizon_effects[term] = effect
        horizon_rows.append(
            {
                "term": term,
                "event_time": horizon,
                "treated_count": len(horizon_treated),
                "treated_tau_sum": format(sum(row["tau"] for row in horizon_treated), "f"),
                "effect": format(effect, "f"),
            }
        )
        for row in horizon_treated:
            weight_rows.append(
                {
                    "row_id": row["row_id"],
                    "unit": row["unit"],
                    "period": row["period"],
                    "event_time": horizon,
                    "tau": format(row["tau"], "f"),
                    "static_weight": format(static_weight, "f"),
                    "static_contribution": format(row["tau"] * static_weight, "f"),
                    "horizon_term": term,
                    "horizon_weight": format(horizon_weight, "f"),
                    "horizon_contribution": format(row["tau"] * horizon_weight, "f"),
                }
            )

    oracle = {
        "status": "success",
        "fixture_id": "F036",
        "source_fixture_id": "F001",
        "source": "hand algebraic oracle from the frozen F001 data-generating process; no R package code is used",
        "generated_at": os.environ.get("DIDBJS_GENERATED_AT", "2026-06-20T08:30:00Z"),
        "input_sha256": sha256(args.input_csv),
        "generator_sha256": sha256(pathlib.Path(__file__)),
        "data_generating_process": {
            "rows": 60,
            "units": 10,
            "periods": 6,
            "treated_units": [1, 2, 3, 4, 5],
            "never_treated_units": [6, 7, 8, 9, 10],
            "treatment_time": 4,
            "untreated_potential_outcome": "Y0 = 10 * i + t",
            "fixed_effect_decomposition": "unit component 10 * i plus time component t",
            "treated_effect": "tau = 1 + event_time + (i - 3) / 10 for D == 1; tau = 0 otherwise",
            "observed_outcome": "Y = Y0 + tau",
            "analytic_weight": "w = 1 for every row",
        },
        "counts": {
            "n_obs": len(rows),
            "n_control": len(control_rows),
            "n_treated": treated_count,
        },
        "oracle": {
            "static_att": float(static_att),
            "treated_tau_sum": float(sum(row["tau"] for row in treated_rows)),
            "static_weight_per_treated_row": float(static_weight),
            "horizon_effects": {term: float(value) for term, value in horizon_effects.items()},
            "horizon_weight_per_treated_row": {
                term: 1 / row["treated_count"] for term, row in zip(horizon_effects, horizon_rows)
            },
        },
        "reference_comparison_policy": {
            "stata": "must match static_att under TOL001",
            "kyle_alias": "must match static_att under TOL001; public idname=i reference error is governed by D016",
            "python": "documented D017 divergence; object shape retained while didbjs core follows static_att",
        },
    }

    (args.metadata_dir / "f036-algebraic-oracle.json").write_text(json.dumps(oracle, indent=2) + "\n")
    write_csv(
        args.metadata_dir / "f036-horizon-effects.csv",
        horizon_rows,
        ["term", "event_time", "treated_count", "treated_tau_sum", "effect"],
    )
    write_csv(
        args.metadata_dir / "f036-treated-weights.csv",
        weight_rows,
        [
            "row_id",
            "unit",
            "period",
            "event_time",
            "tau",
            "static_weight",
            "static_contribution",
            "horizon_term",
            "horizon_weight",
            "horizon_contribution",
        ],
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
