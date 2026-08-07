"""Tests for the dependency-free RBM ASCII output comparator."""

from __future__ import annotations

import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


REPOSITORY_ROOT = Path(__file__).resolve().parents[1]
COMPARATOR = REPOSITORY_ROOT / "tools" / "compare_rbm_outputs.py"


class CompareRbmOutputsTests(unittest.TestCase):
    def run_comparison(
        self, reference_rows: list[str], candidate_rows: list[str], *extra_args: str
    ) -> subprocess.CompletedProcess[str]:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            reference = root / "reference.Temp"
            candidate = root / "candidate.Temp"
            reference.write_text("\n".join(reference_rows) + "\n", encoding="ascii")
            candidate.write_text("\n".join(candidate_rows) + "\n", encoding="ascii")
            return subprocess.run(
                [
                    sys.executable,
                    str(COMPARATOR),
                    str(reference),
                    str(candidate),
                    *extra_args,
                ],
                check=False,
                capture_output=True,
                text=True,
            )

    def test_equal_outputs_pass(self) -> None:
        row = "2000.0042 1 1 1 2 7.83 8.23 -9.70 5.0 5.0"
        result = self.run_comparison([row], [row])

        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("records=1 ordering=equal", result.stdout)
        self.assertIn("max_abs=0", result.stdout)

    def test_difference_above_tolerance_fails(self) -> None:
        reference = "2000.0042 1 1 1 2 7.83 8.23 -9.70 5.0 5.0"
        candidate = "2000.0042 1 1 1 2 7.95 8.23 -9.70 5.0 5.0"
        result = self.run_comparison(
            [reference],
            [candidate],
            "--temperature-tolerance",
            "0.01",
        )

        self.assertEqual(result.returncode, 1)
        self.assertIn("stream_temperature_C: max_abs=0.12", result.stdout)
        self.assertIn("above_tolerance=1", result.stdout)

    def test_ordering_difference_fails(self) -> None:
        reference = "2000.0042 1 1 1 2 7.83 8.23 -9.70 5.0 5.0"
        candidate = "2000.0042 1 1 1 4 7.83 8.23 -9.70 5.0 5.0"
        result = self.run_comparison([reference], [candidate])

        self.assertEqual(result.returncode, 1)
        self.assertIn("FAIL ordering record=1", result.stdout)

    def test_record_count_difference_fails(self) -> None:
        row = "2000.0042 1 1 1 2 7.83 8.23 -9.70 5.0 5.0"
        result = self.run_comparison([row, row], [row])

        self.assertEqual(result.returncode, 1)
        self.assertIn("FAIL record_count candidate_ended_at=1", result.stdout)

    def test_empty_outputs_fail(self) -> None:
        result = self.run_comparison([], [])

        self.assertEqual(result.returncode, 1)
        self.assertIn("FAIL no records found", result.stdout)


if __name__ == "__main__":
    unittest.main()
