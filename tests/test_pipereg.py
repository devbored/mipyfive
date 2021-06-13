import os
import sys
import random
import argparse
import unittest
from nmigen import *
from nmigen.back.pysim import *

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))
from mipyfive.pipereg import *

createVcd = False
outputDir = os.path.join(os.path.dirname(__file__), "..", "out", "pipereg_vcd")
def test_pipereg(value):
    def test(self):
        global createVcd
        global outputDir
        sim = Simulator(self.dut)
        def process():
            yield self.dut.din.eq(value)
            for i in range(2):
                yield Tick()
            self.assertEqual((yield self.dut.dout), value)
        sim.add_clock(1e-6)
        sim.add_sync_process(process)
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
        self.dut = PipeReg(32)

    test_pipereg1  = test_pipereg(0xdeadbeef)
    test_pipereg2  = test_pipereg(0x5a5a5a5a)
    test_pipereg3  = test_pipereg(0x0f0f0f0f)

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--vcd", action="store_true", help="Emit VCD files.")
    args, argv = parser.parse_known_args()
    sys.argv[1:] = argv
    if args.vcd is True:
        print(f"[INFO]: Emitting VCD files to --> {outputDir}\n")
        createVcd = True
    
    unittest.main(verbosity=2)
