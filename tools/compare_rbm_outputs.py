#!/usr/bin/env python3
"""Compare two RBM .Temp files without third-party Python packages."""

from __future__ import annotations

import argparse
import math
from itertools import zip_longest
from pathlib import Path
from typing import Iterator


FIELD_COLUMNS = {
    "stream_temperature_C": 5,
    "headwater_temperature_C": 6,
    "air_temperature_C": 7,
    "qin_cfs": 8,
    "qout_cfs": 9,
}


def records(path: Path) -> Iterator[tuple[tuple[str, ...], list[float]]]:
    with path.open(encoding="ascii") as source:
        for line_number, line in enumerate(source, 1):
            fields = line.split()
            if not fields:
                continue
            if len(fields) < 10:
                raise ValueError(
                    f"{path}:{line_number}: expected at least 10 columns, found {len(fields)}"
                )
            key = tuple(fields[:5])
            try:
                values = [float(value) for value in fields]
            except ValueError as error:
                raise ValueError(f"{path}:{line_number}: non-numeric value") from error
            yield key, values


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("reference", type=Path)
    parser.add_argument("candidate", type=Path)
    parser.add_argument(
        "--temperature-tolerance",
        type=float,
        default=0.01,
        help="maximum allowed absolute temperature difference in C (default: 0.01)",
    )
    parser.add_argument(
        "--flow-tolerance",
        type=float,
        default=0.1,
        help="maximum allowed absolute Qin/Qout difference in cfs (default: 0.1)",
    )
    args = parser.parse_args()

    missing = object()
    totals = {name: 0.0 for name in FIELD_COLUMNS}
    maxima = {name: 0.0 for name in FIELD_COLUMNS}
    above_tolerance = {name: 0 for name in FIELD_COLUMNS}
    count = 0

    for index, pair in enumerate(
        zip_longest(records(args.reference), records(args.candidate), fillvalue=missing), 1
    ):
        reference_record, candidate_record = pair
        if reference_record is missing or candidate_record is missing:
            shorter = "reference" if reference_record is missing else "candidate"
            print(f"FAIL record_count {shorter}_ended_at={index - 1}")
            return 1

        reference_key, reference_values = reference_record
        candidate_key, candidate_values = candidate_record
        if reference_key != candidate_key:
            print(
                f"FAIL ordering record={index} "
                f"reference={reference_key} candidate={candidate_key}"
            )
            return 1

        count = index
        for name, column in FIELD_COLUMNS.items():
            difference = abs(candidate_values[column] - reference_values[column])
            if not math.isfinite(difference):
                print(f"FAIL non-finite difference record={index} field={name}")
                return 1
            tolerance = (
                args.flow_tolerance
                if name in {"qin_cfs", "qout_cfs"}
                else args.temperature_tolerance
            )
            totals[name] += difference
            maxima[name] = max(maxima[name], difference)
            above_tolerance[name] += difference > tolerance

    if count == 0:
        print("FAIL no records found")
        return 1

    failed = False
    print(f"records={count} ordering=equal")
    for name in FIELD_COLUMNS:
        tolerance = (
            args.flow_tolerance
            if name in {"qin_cfs", "qout_cfs"}
            else args.temperature_tolerance
        )
        mean = totals[name] / count if count else 0.0
        above = above_tolerance[name]
        print(
            f"{name}: max_abs={maxima[name]:.10g} mean_abs={mean:.10g} "
            f"above_tolerance={above} tolerance={tolerance:.10g}"
        )
        failed = failed or above > 0
    return int(failed)


if __name__ == "__main__":
    raise SystemExit(main())
