import os
import sys
import argparse
import unittest
from enum import Enum
from nmigen import *
from nmigen.back.pysim import *

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))
from mipyfive.types import *
from mipyfive.compare import *

createVcd = False
outputDir = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "out", "vcd"))
def test_compare(a, b, cmpType, expectedResult):
    def test(self):
        global createVcd
        global outputDir
        sim = Simulator(self.dut)
        def process():
            yield self.dut.in1.eq(a)
            yield self.dut.in2.eq(b)
            yield self.dut.cmpType.eq(cmpType)
            yield Delay(1e-6)

            self.assertEqual((yield self.dut.isTrue), expectedResult)
        sim.add_process(process)
        if createVcd:
            if not os.path.exists(outputDir):
                os.makedirs(outputDir)
            with sim.write_vcd(vcd_file=os.path.join(outputDir, f"{self._testMethodName}.vcd")):
                sim.run()
        else:
            sim.run()
    return test

# Define unit tests
class TestCompare(unittest.TestCase):
    def setUp(self):
        self.dut = CompareUnit(width=32)

    # Test true cases
    test_eq     = test_compare(0x0000000a, 0x0000000a, CompareTypes.EQUAL.value, 1)
    test_neq    = test_compare(0x000000f0, 0x000000ff, CompareTypes.NOT_EQUAL.value, 1)
    test_lt     = test_compare(0x80000000, 0x00000400, CompareTypes.LESS_THAN.value, 1)
    test_ltu    = test_compare(0x00006000, 0x80000000, CompareTypes.LESS_THAN_U.value, 1)
    test_ge     = test_compare(0x70000000, 0x00000070, CompareTypes.GREATER_EQUAL.value, 1)
    test_geu    = test_compare(0x80000000, 0x70000000, CompareTypes.GREATER_EQUAL_U.value, 1)

    # Test false cases
    test_false_eq     = test_compare(0x0000000a, 0x00000005, CompareTypes.EQUAL.value, 0)
    test_false_neq    = test_compare(0x000000ff, 0x000000ff, CompareTypes.NOT_EQUAL.value, 0)
    test_false_lt     = test_compare(0x00000400, 0x80000000, CompareTypes.LESS_THAN.value, 0)
    test_false_ltu    = test_compare(0x80000000, 0x00006000, CompareTypes.LESS_THAN_U.value, 0)
    test_false_ge     = test_compare(0x00000070, 0x70000000, CompareTypes.GREATER_EQUAL.value, 0)
    test_false_geu    = test_compare(0x70000000, 0x80000000, CompareTypes.GREATER_EQUAL_U.value, 0)

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--vcd", action="store_true", help="Emit VCD files.")
    args, argv = parser.parse_known_args()
    sys.argv[1:] = argv
    if args.vcd is True:
        print(f"[INFO]: Emitting VCD files to --> {outputDir}\n")
        createVcd = True
    
    unittest.main(verbosity=2)
