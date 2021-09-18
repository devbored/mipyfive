import os
import sys
import random
import argparse
import unittest
from nmigen import *
from nmigen.back.pysim import *

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))
from tests.utils import *
from mipyfive.utils import *
from mipyfive.types import *
from mipyfive.immgen import *

createVcd = False
outputDir = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "out", "vcd"))
def test_immgen(instruction, expectedImm):
    def test(self):
        global createVcd
        global outputDir
        sim = Simulator(self.dut)
        def process():
            yield self.dut.instruction.eq(instruction)
            yield Delay(1e-6)
            if expectedImm < 0:
                self.assertEqual((yield self.dut.imm), (expectedImm & 0xffffffff))
            else:
                self.assertEqual((yield self.dut.imm), expectedImm)
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
class TestImmgen(unittest.TestCase):
    def setUp(self):
        self.dut = ImmGen()

    randImm12 = random.randint(Imm32Ranges.I_MIN.value, Imm32Ranges.I_MAX.value)
    randImm20 = random.randint(Imm32Ranges.UJ_MIN.value, Imm32Ranges.UJ_MAX.value) & 0xfffff

    test_imm_I_type  = test_immgen(asm2binI("lw", "x5", "x6", str(randImm12)), randImm12)
    test_imm_S_type  = test_immgen(asm2binS("sb", "x1", str(randImm12), "x11"), randImm12)
    test_imm_B_type  = test_immgen(asm2binB("bne", "x3", str(randImm12), "x3"), randImm12 & ~1)
    test_imm_U_type  = test_immgen(asm2binU("lui", "x10", str(randImm20)), randImm20 << 12)
    test_imm_J_type  = test_immgen(asm2binJ("jal", "x17", str(randImm20)), randImm20 & ~1)

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--vcd", action="store_true", help="Emit VCD files.")
    parser.add_argument("-v", dest="verbosity", type=int, default=2, help="Verbosity level.")
    args, argv = parser.parse_known_args()
    sys.argv[1:] = argv
    if args.vcd is True:
        print(f"[mipyfive - Info]: Emitting VCD files to --> {outputDir}\n")
        createVcd = True

    unittest.main(verbosity=args.verbosity)
