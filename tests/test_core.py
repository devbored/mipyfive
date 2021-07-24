import os
import sys
import argparse
import unittest
from nmigen import *
from nmigen.back.pysim import *

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))
from tests.utils import *
from mipyfive.utils import *
from mipyfive.core import *
from mipyfive.types import *
from examples.common.ram import *

createVcd = False
outputDir = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "out", "vcd"))
def test_core(program):
    def test(self):
        global createVcd
        global outputDir
        sim = Simulator(self.dut)
        def process():
            # NOTE/TODO: No assertions for now - add later
            # (i.e. test(s) will always pass - currently just using this for VCD dumping)
            for i in range(len(program)):
                yield self.dut.submodules.imem.memory[i].eq(program[i])

            for i in range(len(program)*2):
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
        self.dut  = Module()
        self.dut.submodules.core = MipyfiveCore(dataWidth=32, regCount=32, pcStart=-4, ISA=CoreISAconfigs.RV32I.value)
        self.dut.submodules.imem = RAM(width=32, depth=128, wordAligned=True)
        self.dut.submodules.dmem = RAM(width=32, depth=128)

        self.dut.d.comb += [
            # imem connections
            self.dut.submodules.imem.writeEnable.eq(0),
            self.dut.submodules.imem.writeData.eq(0),
            self.dut.submodules.imem.readAddr.eq(self.dut.submodules.core.PCout),
            self.dut.submodules.imem.writeAddr.eq(0),
            # dmem connections
            self.dut.submodules.dmem.writeEnable.eq(self.dut.submodules.core.DataWE),
            self.dut.submodules.dmem.writeData.eq(self.dut.submodules.core.DataOut),
            self.dut.submodules.dmem.readAddr.eq(self.dut.submodules.core.DataAddr),
            self.dut.submodules.dmem.writeAddr.eq(self.dut.submodules.core.DataAddr),
            # core connections
            self.dut.submodules.core.instruction.eq(self.dut.submodules.imem.readData),
            self.dut.submodules.core.DataIn.eq(self.dut.submodules.dmem.readData)
        ]

    program = '''
        addi  x1, x0, 6
        sw    x1, 0,  x0
        add   x2, x1, x1
        slli  x3, x2, 1
        addi  x3, x3, 1
        add   x0, x0, x0
        addi  x3, x3, 11
        addi  x3, x3, 11
        beq   x3, -2, x3
        addi  x3, x3, 11
        addi  x3, x3, 11
    '''
    programBinary = asm2Bin(program)
    print(programBinary)

    test_core = test_core(programBinary)

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--vcd", action="store_true", help="Emit VCD files.")
    args, argv = parser.parse_known_args()
    sys.argv[1:] = argv
    if args.vcd is True:
        print(f"[INFO]: Emitting VCD files to --> {outputDir}\n")
        createVcd = True

    unittest.main(verbosity=2)
