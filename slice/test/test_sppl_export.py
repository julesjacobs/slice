#!/usr/bin/env python3
"""Smoke-test generated distribution statements with SPPL 2.0.4.

Run with a Python environment containing sppl==2.0.4:

    python test/test_sppl_export.py _build/default/bin/main.exe
"""

import pathlib
import subprocess
import sys
import tempfile

import sppl
from sppl.compilers.sppl_to_python import SPPL_Compiler


SOURCES = (
    "gaussian(0.12345678901234566, 1.2345678901234567)",
    "exponential(3)",
    "lognormal(0.6931471805599453, 0.12345678901234566)",
    "gamma(1.2345678901234567, 2.3456789012345678)",
    "weibull(2, 3)",
    "pareto(2, 3)",
    "gumbel1(2, 4)",
    "gumbel2(2, 4)",
    "exppow(2, 3)",
    "binomial(0.25, 8)",
)


def main() -> None:
    if len(sys.argv) != 2:
        raise SystemExit(f"usage: {sys.argv[0]} PATH_TO_SLICE_MAIN")

    executable = pathlib.Path(sys.argv[1]).resolve()
    if sppl.__version__ != "2.0.4":
        raise AssertionError(f"expected SPPL 2.0.4, found {sppl.__version__}")
    with tempfile.TemporaryDirectory() as directory:
        source_file = pathlib.Path(directory) / "smoke.slice"
        for source in SOURCES:
            source_file.write_text(source, encoding="utf-8")
            generated = subprocess.run(
                [executable, "--to-sppl", source_file],
                check=True,
                capture_output=True,
                text=True,
            ).stdout
            module = SPPL_Compiler(generated).execute_module()
            if not hasattr(module, "model"):
                raise AssertionError(f"SPPL did not construct model for {source!r}")


if __name__ == "__main__":
    main()
