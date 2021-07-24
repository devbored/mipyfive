import os
import sys
import argparse
import unittest
import textwrap
from nmigen import *
from nmigen.back.pysim import *

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))
from tests.utils import *
from mipyfive.utils import *
from mipyfive.core import *
from mipyfive.types import *

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
        self.dut = MipyfiveCore(dataWidth=32, regCount=32, pcStart=0, ISA=CoreISAconfigs.RV32I.value)

    # Test program
    program = '''
        addi  x1, x0, 6
        add   x2, x1, x1
        slli  x3, x2, 1
        addi  x3, x3, 1
        beq   x3, -2, x3
        add   x0, x0, x0
        addi  x3, x3, 11
        addi  x3, x3, 11
    '''
    binary = asm2Bin(program)

    test_core    = test_core(binary, 25)

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--vcd", action="store_true", help="Emit VCD files.")
    args, argv = parser.parse_known_args()
    sys.argv[1:] = argv
    if args.vcd is True:
        print(f"[INFO]: Emitting VCD files to --> {outputDir}\n")
        createVcd = True

    unittest.main(verbosity=2)
