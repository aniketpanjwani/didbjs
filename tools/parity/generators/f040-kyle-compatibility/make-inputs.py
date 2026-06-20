#!/usr/bin/env python3
"""Create F040 Kyle-compatibility inputs from the verified F015 pretrend panel."""

from __future__ import annotations

import argparse
import csv
import pathlib


FIELDS = [
    "row_id",
    "unit",
    "t",
    "Ei",
    "D",
    "event_time",
    "Y0",
    "tau",
    "Y",
    "clust",
    "x1",
    "x2",
    "w",
    "wtr_early",
    "wtr_late",
    "Y2",
]


def read_rows(path: pathlib.Path) -> list[dict[str, str]]:
    with path.open(newline="") as f:
        return list(csv.DictReader(f))


def write_rows(path: pathlib.Path, rows: list[dict[str, object]], fields: list[str]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fields, lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)


def augment_rows(source: list[dict[str, str]]) -> list[dict[str, object]]:
    rows: list[dict[str, object]] = []
    for row in source:
        unit = int(row["unit"])
        period = int(row["t"])
        treated = row["D"] == "1"
        y = float(row["Y"])
        rows.append(
            {
                "row_id": row["row_id"],
                "unit": unit,
                "t": period,
                "Ei": row["Ei"],
                "D": row["D"],
                "event_time": row["event_time"],
                "Y0": row["Y0"],
                "tau": row["tau"],
                "Y": row["Y"],
                "clust": unit,
                "x1": ((unit + period) % 3) - 1,
                "x2": ((2 * unit + period) % 4) - 1.5,
                "w": 1,
                "wtr_early": 1 if treated and unit <= 5 else 0,
                "wtr_late": 1 if treated and unit > 5 else 0,
                "Y2": f"{2 * y + (1.5 if treated else 0):.12f}",
            }
        )
    return rows


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("f015_input", type=pathlib.Path)
    parser.add_argument("output_dir", type=pathlib.Path)
    args = parser.parse_args()

    rows = augment_rows(read_rows(args.f015_input))
    write_rows(args.output_dir / "panel.csv", rows, FIELDS)

    collision_rows = []
    for row in rows:
        copy = dict(row)
        copy["i"] = copy.pop("unit")
        collision_rows.append(copy)
    write_rows(args.output_dir / "id-collision.csv", collision_rows, ["row_id", "i", *FIELDS[2:]])
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
