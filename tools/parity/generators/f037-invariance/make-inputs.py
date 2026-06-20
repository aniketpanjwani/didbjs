#!/usr/bin/env python3
"""Create deterministic F037 transformation-invariance input panels."""

from __future__ import annotations

import argparse
import csv
import pathlib


SCENARIOS = (
    "base",
    "row_permuted",
    "unit_relabel",
    "time_shift",
    "outcome_scaled",
    "constant_shift",
    "weight_scaled",
)


def read_rows(path: pathlib.Path) -> list[dict[str, str]]:
    with path.open(newline="") as f:
        return list(csv.DictReader(f))


def write_rows(path: pathlib.Path, rows: list[dict[str, object]]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    fields = ["row_id", "unit", "t", "Ei", "D", "event_time", "Y0", "tau", "Y", "w"]
    with path.open("w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fields, lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)


def base_rows(source: list[dict[str, str]]) -> list[dict[str, object]]:
    rows: list[dict[str, object]] = []
    for row in source:
        rows.append(
            {
                "row_id": row["row_id"],
                "unit": int(row["i"]),
                "t": int(row["t"]),
                "Ei": "" if row["Ei"] == "" else int(row["Ei"]),
                "D": int(row["D"]),
                "event_time": row["event_time"],
                "Y0": float(row["Y0"]),
                "tau": float(row["tau"]),
                "Y": float(row["Y"]),
                "w": float(row["w"]),
            }
        )
    return rows


def transform(rows: list[dict[str, object]], scenario: str) -> list[dict[str, object]]:
    out = [dict(row) for row in rows]
    if scenario == "base":
        return out
    if scenario == "row_permuted":
        return sorted(out, key=lambda row: (int(row["t"]) % 2, -int(row["unit"]), int(row["t"])))
    if scenario == "unit_relabel":
        for row in out:
            row["unit"] = int(row["unit"]) * 10 + 7
        return out
    if scenario == "time_shift":
        for row in out:
            row["t"] = int(row["t"]) + 100
            if row["Ei"] != "":
                row["Ei"] = int(row["Ei"]) + 100
        return out
    if scenario == "outcome_scaled":
        scale = 3.5
        for row in out:
            row["Y0"] = float(row["Y0"]) * scale
            row["tau"] = float(row["tau"]) * scale
            row["Y"] = float(row["Y"]) * scale
        return out
    if scenario == "constant_shift":
        shift = 7.25
        for row in out:
            row["Y0"] = float(row["Y0"]) + shift
            row["Y"] = float(row["Y"]) + shift
        return out
    if scenario == "weight_scaled":
        for row in out:
            row["w"] = float(row["w"]) * 10
        return out
    raise ValueError(f"unknown scenario {scenario}")


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("f001_input", type=pathlib.Path)
    parser.add_argument("output_dir", type=pathlib.Path)
    args = parser.parse_args()

    rows = base_rows(read_rows(args.f001_input))
    for scenario in SCENARIOS:
        write_rows(args.output_dir / f"{scenario}.csv", transform(rows, scenario))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
