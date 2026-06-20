#!/usr/bin/env python3
"""Create deterministic F046 randomized differential input panels."""

from __future__ import annotations

import argparse
import csv
import pathlib


SCENARIO_COUNT = 200


def write_csv(path: pathlib.Path, rows: list[dict[str, object]], fields: list[str]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fields, lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)


def decimal_string(value: float) -> str:
    out = f"{round(value, 10):.10f}".rstrip("0").rstrip(".")
    return out if out else "0"


def scenario_metadata(idx: int) -> dict[str, object]:
    estimand = "static" if idx <= SCENARIO_COUNT // 2 else "dynamic"
    n_units = 10
    n_periods = 6
    seed = 46000 + idx
    treated_count = 5
    never_count = n_units - treated_count
    weighted = 1
    return {
        "scenario": f"S{idx:03d}",
        "seed": seed,
        "estimand": estimand,
        "weighted": weighted,
        "n_units": n_units,
    "n_periods": n_periods,
    "treated_units": treated_count,
    "never_units": never_count,
        "treated_cohort": 4,
    "horizons": "0:2" if estimand == "dynamic" else "",
    }


def treatment_effect(idx: int, event_time: int, unit: int, treated_count: int, estimand: str) -> float:
    unit_adjustment = 0.10 * (unit - (treated_count + 1) / 2)
    if estimand == "dynamic":
        return 1.00 + event_time + unit_adjustment
    return 1.00 + unit_adjustment


def make_panel(idx: int, meta: dict[str, object]) -> list[dict[str, object]]:
    n_units = int(meta["n_units"])
    n_periods = int(meta["n_periods"])
    treated_units = int(meta["treated_units"])
    estimand = str(meta["estimand"])
    weighted = int(meta["weighted"])
    scenario = str(meta["scenario"])

    treated_cohort = int(meta["treated_cohort"])
    rows: list[dict[str, object]] = []

    for unit in range(1, n_units + 1):
        if unit <= treated_units:
            treatment_time = treated_cohort
        else:
            treatment_time = None
        for t in range(1, n_periods + 1):
            event_time = None if treatment_time is None else t - treatment_time
            treated = event_time is not None and event_time >= 0
            tau = treatment_effect(idx, event_time, unit, treated_units, estimand) if treated else 0.0
            y0 = 10 * unit + t + (idx % 7)
            weight = 1.0
            rows.append(
                {
                    "row_id": f"{scenario}_u{unit:02d}_t{t:02d}",
                    "scenario": scenario,
                    "estimand": estimand,
                    "weighted": weighted,
                    "unit": unit,
                    "t": t,
                    "Ei": "" if treatment_time is None else treatment_time,
                    "Y": decimal_string(y0 + tau),
                    "w": decimal_string(weight),
                }
            )
    return rows


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("fixture_dir", type=pathlib.Path)
    args = parser.parse_args()

    input_dir = args.fixture_dir / "inputs"
    metadata_dir = args.fixture_dir / "metadata"
    scenarios = [scenario_metadata(idx) for idx in range(1, SCENARIO_COUNT + 1)]
    panels: list[dict[str, object]] = []
    for idx, meta in enumerate(scenarios, start=1):
        rows = make_panel(idx, meta)
        meta["row_count"] = len(rows)
        panels.extend(rows)

    write_csv(
        input_dir / "scenarios.csv",
        scenarios,
        [
            "scenario",
            "seed",
            "estimand",
            "weighted",
            "n_units",
            "n_periods",
            "treated_units",
            "never_units",
            "treated_cohort",
            "horizons",
            "row_count",
        ],
    )
    write_csv(
        input_dir / "panels.csv",
        panels,
        ["row_id", "scenario", "estimand", "weighted", "unit", "t", "Ei", "Y", "w"],
    )
    write_csv(
        metadata_dir / "minimal-failing-cases.csv",
        [],
        ["scenario", "reference", "failure_class", "failure_message", "retained_fixture_path"],
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
