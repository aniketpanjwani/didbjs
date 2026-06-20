#!/usr/bin/env python3
"""Generate F039 saved-artifact row-mapping inputs."""

from __future__ import annotations

import argparse
import csv
import pathlib


def read_rows(path: pathlib.Path) -> tuple[list[str], list[dict[str, str]]]:
    with path.open(newline="") as f:
        reader = csv.DictReader(f)
        return list(reader.fieldnames or []), list(reader)


def write_rows(path: pathlib.Path, fields: list[str], rows: list[dict[str, str]]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fields)
        writer.writeheader()
        writer.writerows(rows)


def add_second_outcome(rows: list[dict[str, str]]) -> list[dict[str, str]]:
    out = []
    for row in rows:
        updated = dict(row)
        updated["Y2"] = f"{float(row['Y']) + 0.5 * float(row['tau']) + 0.1 * float(row['unit']):.12g}"
        out.append(updated)
    return out


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("source", type=pathlib.Path)
    parser.add_argument("output_dir", type=pathlib.Path)
    args = parser.parse_args()

    fields, rows = read_rows(args.source)
    if "Y2" not in fields:
      fields = fields + ["Y2"]
    base = add_second_outcome(rows)
    write_rows(args.output_dir / "base.csv", fields, base)

    reordered = sorted(base, key=lambda row: (int(row["t"]) % 2, -int(row["unit"]), int(row["t"])))
    write_rows(args.output_dir / "reordered.csv", fields, reordered)

    modified = [dict(row) for row in base]
    for row in modified:
        if row["unit"] == "1":
            row["Ei"] = "5"
    write_rows(args.output_dir / "modified.csv", fields, modified)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
