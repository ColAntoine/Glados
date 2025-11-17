import subprocess
import pathlib
import pytest

ROOT = pathlib.Path(__file__).parent
BIN = ROOT.parent / "glados"

cases = [
    ("foo.scm", 0, "42"),
    ("error.scm", 84, "*** ERROR : variable foo is not bound."),
    ("call.scm", 0, "5"),
    ("lambda1.scm", 0, "#<procedure>"),
    ("lambda2.scm", 0, "3"),
    ("lambda3.scm", 0, "7"),
    ("function1.scm", 0, "7"),
    ("if1.scm", 0, "1"),
    ("if2.scm", 0, "2"),
    ("if3.scm", 0, "21"),
    ("builtins1.scm", 0, "11"),
    ("builtins2.scm", 0, "#t"),
    ("builtins3.scm", 0, "#f"),
    ("superior.scm", 0, "#t"),
    ("factorial.scm", 0, "3628800"),
]

@pytest.mark.parametrize("fname,rc,expected", cases)
def test_case(fname, rc, expected):
    path = ROOT / "cases" / fname
    proc = subprocess.run([str(BIN)], input=path.read_bytes(), stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    out = proc.stdout.decode().strip()
    assert proc.returncode == rc, f"{fname}: return code {proc.returncode} != {rc}\noutput:\n{out}"
    if expected:
        assert out == expected, f"{fname}: output mismatch\nexpected:\n{expected}\nactual:\n{out}"
