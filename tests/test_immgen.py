import os
import sys
import random
import argparse
import unittest
from nmigen import *
from nmigen.back.pysim import *

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))
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

    test_imm1  = test_immgen(0x01900293, 0x19)
    test_imm2  = test_immgen(0xff500313, 0xfffffff5)
    test_imm3  = test_immgen(0x00602223, 0x4)
    test_imm4  = test_immgen(0x004003b7, 0x400000)
    test_imm5  = test_immgen(0x00306393, 0x3)
    test_imm6  = test_immgen(0x008004ef, 0x8)
    test_imm7  = test_immgen(0xfe0002e3, 0xffffffe4)

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--vcd", action="store_true", help="Emit VCD files.")
    args, argv = parser.parse_known_args()
    sys.argv[1:] = argv
    if args.vcd is True:
        print(f"[INFO]: Emitting VCD files to --> {outputDir}\n")
        createVcd = True
    
    unittest.main(verbosity=2)
