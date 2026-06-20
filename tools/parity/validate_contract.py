#!/usr/bin/env python3
"""Validate RC conformance contract cross-references and terminal evidence states."""

from __future__ import annotations

import csv
import json
import pathlib
import re
import sys


ROOT = pathlib.Path(__file__).resolve().parents[2]
MATRIX = ROOT / "inst/spec/feature-matrix.csv"
VERIFICATION = ROOT / "docs/verification-criteria.md"
TOLERANCES = ROOT / "docs/tolerance-registry-v1.md"


def fail(message: str) -> None:
    print(f"CONTRACT_INVALID: {message}", file=sys.stderr)
    raise SystemExit(1)


def main() -> None:
    defined_features = set(
        re.findall(r"^### (F\d{3}) ", VERIFICATION.read_text(), flags=re.MULTILINE)
    )
    defined_tolerances = set(re.findall(r"`((?:TOL\d{3})|EXACT)`", TOLERANCES.read_text()))
    rows = list(csv.DictReader(MATRIX.open(newline="")))

    matrix_features = {row["feature_id"] for row in rows if row["feature_id"].startswith("F")}
    missing_in_matrix = sorted(defined_features - matrix_features)
    missing_in_docs = sorted(matrix_features - defined_features)
    if missing_in_matrix or missing_in_docs:
        fail(
            "feature mismatch "
            + json.dumps(
                {
                    "missing_in_matrix": missing_in_matrix,
                    "missing_in_docs": missing_in_docs,
                },
                sort_keys=True,
            )
        )

    bad_rows = []
    for row in rows:
        feature_id = row["feature_id"]
        for field in (
            "normative_source",
            "source_anchor",
            "expected_behavior",
            "fixture_ids",
            "test_ids",
            "artifact_paths",
            "tolerance_id",
            "allowed_terminal_status",
            "status",
        ):
            if row[field] in {"", "TBD"}:
                bad_rows.append([feature_id, field, row[field]])
        if row["mandatory"].lower() == "true":
            if row["allowed_terminal_status"] != "parity-verified|approved-divergence":
                bad_rows.append([feature_id, "allowed_terminal_status", row["allowed_terminal_status"]])
            if row["status"] not in {"parity-verified", "approved-divergence"}:
                bad_rows.append([feature_id, "status", row["status"]])
            if row["status"] == "approved-divergence" and row["decision_record_id"] in {"", "NA"}:
                bad_rows.append([feature_id, "decision_record_id", row["decision_record_id"]])
        if row["tolerance_id"] not in defined_tolerances:
            bad_rows.append([feature_id, "tolerance_id", row["tolerance_id"]])

    if bad_rows:
        fail("bad matrix rows " + json.dumps(bad_rows, sort_keys=True))

    print(
        json.dumps(
            {
                "status": "ok",
                "rows": len(rows),
                "feature_rows": len(matrix_features),
                "tolerance_ids": sorted(defined_tolerances),
            },
            sort_keys=True,
        )
    )


if __name__ == "__main__":
    main()
