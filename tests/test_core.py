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
from mipyfive.core import *

createVcd = False
outputDir = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "out", "vcd"))
def test_core(program, dataIn):
    def test(self):
        global createVcd
        global outputDir
        sim = Simulator(self.dut)
        def process():
            yield self.dut.DataIn.eq(dataIn)
            
            # NOTE/TODO: No assertions for now - add later
            # (i.e. test(s) will always pass - currently just using this for VCD dumping)
            for i in range(len(program)):
                pcVal = yield self.dut.PCout
                yield self.dut.instruction.eq(program[(pcVal // 4) % len(program)])
                yield Tick()
            for i in range(len(program)):
                yield Tick()

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
class TestCore(unittest.TestCase):
    def setUp(self):
        self.dut = MipyfiveCore(dataWidth=32, regCount=32)

    # Test program
    program = [
        asm2binI("addi", "x1", "6", "x0"),
        asm2binR("add", "x2", "x1", "x1"),
        asm2binI("slli", "x3", "1", "x2"),
        asm2binI("addi", "x3", "11", "x3"),
        asm2binB("beq", "x3", "-2", "x3"),
        asm2binR("add", "x0", "x0", "x0"),
        asm2binI("addi", "x3", "11", "x3"),
        asm2binI("addi", "x3", "11", "x3")
    ]
    test_core    = test_core(program, 25)

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--vcd", action="store_true", help="Emit VCD files.")
    args, argv = parser.parse_known_args()
    sys.argv[1:] = argv
    if args.vcd is True:
        print(f"[INFO]: Emitting VCD files to --> {outputDir}\n")
        createVcd = True
    
    unittest.main(verbosity=2)
